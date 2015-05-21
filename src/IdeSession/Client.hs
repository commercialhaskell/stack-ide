{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}
module IdeSession.Client
    ( ClientIO(..)
    , startEmptySession
#ifdef USE_CABAL
    , startCabalSession
    , sendTargetsList
#endif
    ) where

import Control.Applicative ((<$>))
import Control.Arrow ((***))
import Control.Concurrent.Async (withAsync)
import Control.Exception
import Control.Monad (join, mfilter, void)
import Data.Aeson (Value)
import Data.Function
import Data.IORef
import Data.List (sortBy)
import Data.Monoid
import Data.Ord
import Prelude hiding (mod, span)

import Data.Text (Text)
import qualified Data.Text as Text

import IdeSession
import IdeSession.Client.AnnotateHaskell (annotateType, Autocomplete)
import IdeSession.Client.AnnotateMessage (annotateMessage)
import IdeSession.Client.CmdLine
import IdeSession.Client.JsonAPI
#ifdef USE_CABAL
import IdeSession.Client.Cabal
#endif

data ClientIO = ClientIO
    { putJson :: Value -> IO ()
    , getJson :: IO Value
    }

startEmptySession :: ClientIO -> Options -> EmptyOptions -> IO ()
startEmptySession clientIO Options{..} EmptyOptions = do
    sendWelcome clientIO
    bracket (initSession optInitParams optConfig)
            shutdownSession
            (mainLoop clientIO)

#ifdef USE_CABAL
startCabalSession :: ClientIO -> Options -> CabalOptions -> IO ()
startCabalSession clientIO options cabalOptions = do
    sendWelcome clientIO
    bracket (initCabalSession options cabalOptions)
            shutdownSession
            (mainLoop clientIO)

sendTargetsList :: ClientIO -> FilePath -> IO ()
sendTargetsList clientIO fp = do
    sendWelcome clientIO
    putJson clientIO . toJSON =<< listTargets fp
#endif

sendWelcome :: ClientIO -> IO ()
sendWelcome clientIO =
    putJson clientIO $ toJSON $ ResponseWelcome ideBackendClientVersion

-- | Version of the client API
--
-- This should be incremented whenever we make a change that editors might need
-- to know about.
ideBackendClientVersion :: VersionInfo
ideBackendClientVersion = VersionInfo 0 1 0

{-------------------------------------------------------------------------------
  Main loop

  Assumes the session has been properly initialized
-------------------------------------------------------------------------------}

mainLoop :: ClientIO -> IdeSession -> IO ()
mainLoop clientIO session0 = do
  updateSession session0 (updateCodeGeneration True) ignoreProgress
  mprocessRef <- newIORef Nothing
  go session0 mprocessRef
  where
    putEnc = putJson clientIO . toJSON
    -- This is called after every session update that doesn't yield
    -- compile errors.  If there are compile errors, we use the info
    -- from the previous update.
    go :: IdeSession -> IORef (Maybe (RunActions RunResult)) -> IO ()
    go session mprocessRef = do
      spanInfo <- getSpanInfo session -- Might not be empty (for Cabal init)
      fileMap <- getFileMap session
      expTypes <- getExpTypes session
      autoComplete <- getAutocompletion session
      fix $ \loop -> do
        value <- getJson clientIO
        case fromJSON value of
          Left err -> do
            putEnc $ ResponseInvalidRequest err
            loop
          Right (RequestUpdateSession upd) -> do
            updateSession session (mconcat (map makeSessionUpdate upd)) $ \progress ->
              putEnc $ ResponseUpdateSession (Just progress)
            putEnc $ ResponseUpdateSession Nothing
            errors <- getSourceErrors session
            if all ((== KindWarning) . errorKind) errors
              then go session mprocessRef
              else loop
          Right RequestGetSourceErrors -> do
            errors <- getSourceErrors session
            putEnc $ ResponseGetSourceErrors errors
            loop
          Right RequestGetAnnSourceErrors -> do
            errors <- getSourceErrors session
            putEnc $ ResponseGetAnnSourceErrors $ map annotateMessage errors
            loop
          Right RequestGetLoadedModules -> do
            mods <- getLoadedModules session
            putEnc $ ResponseGetLoadedModules mods
            loop
          Right (RequestGetSpanInfo span) -> do
            case fileMap (spanFilePath span) of
              Just mod -> do
                let mkInfo (span', info) = ResponseSpanInfo info span'
                putEnc $ ResponseGetSpanInfo
                       $ map mkInfo
                       $ spanInfo (moduleName mod) span
              Nothing ->
                putEnc $ ResponseGetSpanInfo []
            loop
          Right (RequestGetExpTypes span) -> do
            putEnc $ ResponseGetExpTypes $
              case fileMap (spanFilePath span) of
                Just mod ->
                  map (\(span', info) -> ResponseExpType info span') $
                  sortSpans $
                  expTypes (moduleName mod) span
                Nothing -> []
            loop
          Right (RequestGetAnnExpTypes span) -> do
            putEnc $ ResponseGetAnnExpTypes $
              case fileMap (spanFilePath span) of
                Just (moduleName -> mn) ->
                  map (annotateTypeInfo (autoComplete mn)) $
                  sortSpans $
                  expTypes mn span
                Nothing -> []
            loop
          Right (RequestGetAutocompletion autocmpletionSpan) -> do
            case fileMap (autocompletionFilePath autocmpletionSpan) of
              Just mod -> do
                let query = (autocompletionPrefix autocmpletionSpan)
                    splitQualifier = join (***) reverse . break (== '.') . reverse
                    (prefix, qualifierStr) = splitQualifier query
                    qualifier = mfilter (not . Text.null) (Just (Text.pack qualifierStr))
                putEnc $ ResponseGetAutocompletion
                       $ filter ((== qualifier) . autocompletionQualifier)
                       $ map idInfoToAutocompletion
                       $ autoComplete (moduleName mod) prefix
              Nothing ->
                putEnc $ ResponseGetAutocompletion []
            loop
          Right (RequestRun usePty mn identifier) -> do
            actions <- (if usePty then runStmtPty else runStmt)
              session (Text.unpack mn) (Text.unpack identifier)
            writeIORef mprocessRef (Just actions)
            let sendOutput = fix $ \outputLoop -> do
                  result <- runWait actions
                  case result of
                    Left output -> do
                      putEnc $ ResponseProcessOutput output
                      outputLoop
                    Right done -> do
                      putEnc $ ResponseProcessDone done
                      writeIORef mprocessRef Nothing
            void $ withAsync sendOutput $ \_ -> loop
          Right (RequestProcessInput input) -> do
            mprocess <- readIORef mprocessRef
            case mprocess of
              Just actions -> supplyStdin actions input
              Nothing -> putEnc ResponseNoProcessError
            loop
          Right RequestProcessKill -> do
            mprocess <- readIORef mprocessRef
            case mprocess of
              Just actions -> interrupt actions
              Nothing -> putEnc ResponseNoProcessError
            loop
          Right RequestShutdownSession ->
            putEnc $ ResponseShutdownSession
    ignoreProgress :: Progress -> IO ()
    ignoreProgress _ = return ()

annotateTypeInfo :: Autocomplete -> (SourceSpan, Text) -> ResponseAnnExpType
annotateTypeInfo autocomplete (span, info) =
  ResponseAnnExpType (TypeIdInfo <$> annotateType autocomplete info) span

-- | We sort the spans from thinnest to thickest. Currently
-- ide-backend sometimes returns results unsorted, therefore for now
-- we do the sort here, and in future ide-backend can be changed to do
-- this.
sortSpans :: [(SourceSpan,a)] -> [(SourceSpan,a)]
sortSpans = sortBy (on thinner fst)
  where thinner x y =
          comparing (if on (==) spanFromLine x y &&
                        on (==) spanToLine x y
                        then \(SourceSpan _ _ s _ e) -> e - s
                        else \(SourceSpan _ s _ e _) -> e - s)
                    x
                    y

-- | Construct autocomplete information
idInfoToAutocompletion :: IdInfo -> AutocompletionInfo
idInfoToAutocompletion IdInfo{idProp = IdProp{idName, idDefinedIn, idType}, idScope} =
  AutocompletionInfo definedIn idName qualifier idType
  where definedIn = moduleName idDefinedIn
        qualifier = case idScope of
                     Binder                 -> Nothing
                     Local{}                -> Nothing
                     Imported{idImportQual} -> mfilter (not . Text.null) (Just idImportQual)
                     WiredIn                -> Nothing

makeSessionUpdate :: RequestSessionUpdate -> IdeSessionUpdate
makeSessionUpdate (RequestUpdateSourceFile filePath contents) =
  updateSourceFile filePath contents
makeSessionUpdate (RequestUpdateSourceFileFromFile filePath) =
  updateSourceFileFromFile filePath
makeSessionUpdate (RequestUpdateGhcOpts options) =
  updateGhcOpts options
