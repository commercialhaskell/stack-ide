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

import Control.Exception
import Control.Monad (join, mfilter)
import Control.Arrow ((***))
import Data.Function
import Data.List (sortBy)
import Data.Monoid
import Data.Ord
import Data.Text (Text)
import Prelude hiding (mod, span)
import System.IO
import Data.Aeson (Value)

import qualified Data.Text as Text

import IdeSession
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
  go session0
  where
    putEnc = putJson clientIO . toJSON
    -- This is called after every session update that doesn't yield
    -- compile errors.  If there are compile errors, we use the info
    -- from the previous update.
    go :: IdeSession -> IO ()
    go session = do
      spanInfo <- getSpanInfo session -- Might not be empty (for Cabal init)
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
              then go session
              else loop
          Right RequestGetSourceErrors -> do
            errors <- getSourceErrors session
            putEnc $ ResponseGetSourceErrors errors
            loop
          Right RequestGetLoadedModules -> do
            mods <- getLoadedModules session
            putEnc $ ResponseGetLoadedModules mods
            loop
          Right (RequestGetSpanInfo span) -> do
            fileMap <- getFileMap session
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
            fileMap <- getFileMap session
            case fileMap (spanFilePath span) of
              Just mod -> do
                let mkInfo (span', info) = ResponseExpType info span'
                putEnc $ ResponseGetExpTypes
                      $ map mkInfo
                      $ sortSpans
                      $ expTypes (moduleName mod) span
              Nothing ->
                putEnc $ ResponseGetExpTypes []
            loop
          Right (RequestGetAutocompletion autocmpletionSpan) -> do
            fileMap <- getFileMap session
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
          Right RequestShutdownSession ->
            putEnc $ ResponseShutdownSession
    ignoreProgress :: Progress -> IO ()
    ignoreProgress _ = return ()

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
