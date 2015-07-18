{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}
module Stack.Ide
    ( ClientIO(..)
    , startEmptySession
    , sendExceptions
    ) where

import           Control.Applicative ((<$>))
import           Control.Arrow ((***))
import           Control.Concurrent.Async (withAsync)
import           Control.Exception
import           Control.Monad (join, mfilter, void)
import qualified Data.ByteString.Char8 as S8
import           Data.Function
import           Data.IORef
import           Data.List (sortBy)
import           Data.Monoid
import           Data.Ord
import           Data.Text (Text)
import qualified Data.Text as Text
import           IdeSession
import           IdeSession.Util.Logger
import           Prelude hiding (mod, span)
import           Stack.Ide.AnnotateHaskell (annotateType, Autocomplete)
import           Stack.Ide.CmdLine
import           Stack.Ide.JsonAPI
import           System.Environment (getEnvironment)
import           System.Exit (exitWith, exitFailure)

data ClientIO = ClientIO
    { sendResponse :: Sequenced Response -> IO ()
    , receiveRequest :: IO (Either String (Sequenced Request))
    , logMessage :: LogFunc
    }

startEmptySession :: ClientIO -> Options -> IO ()
startEmptySession clientIO Options{..} = do
    let callbacks = IdeCallbacks
            { ideCallbacksLogFunc = logMessage clientIO }
    sendWelcome clientIO
    bracket (initSessionWithCallbacks callbacks optInitParams optConfig)
            shutdownSession
            (mainLoop clientIO)

sendWelcome :: ClientIO -> IO ()
sendWelcome clientIO =
    sendResponse clientIO $ NoSeq $ ResponseWelcome ideBackendClientVersion

sendExceptions :: ClientIO -> IO () -> IO ()
sendExceptions clientIO inner =
  inner `catch` \ex ->
    case fromException ex of
      Just ec -> exitWith ec
      Nothing -> do
        sendResponse clientIO $ NoSeq $ ResponseFatalError (show ex)
        exitFailure

-- | Version of the client API
--
-- This should be incremented whenever we make a change that editors might need
-- to know about.
ideBackendClientVersion :: VersionInfo
ideBackendClientVersion = VersionInfo 0 1 1

{-------------------------------------------------------------------------------
  Main loop

  Assumes the session has been properly initialized
-------------------------------------------------------------------------------}

mainLoop :: ClientIO -> IdeSession -> IO ()
mainLoop clientIO session0 = do
  updateSession session0 (updateCodeGeneration True) ignoreStatus
  mprocessRef <- newIORef Nothing
  go session0 mprocessRef
  where
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
        ereq <- pullSeq <$> receiveRequest clientIO
        let send = sendResponse clientIO . withSameSeqAs ereq
        case unsequenced ereq of
          Left err -> do
            send $ ResponseInvalidRequest err
            loop
          Right (RequestUpdateSession upd) -> do
            updateSession session (mconcat (map makeSessionUpdate upd)) $ \status ->
              send $ ResponseUpdateSession status
            errors <- getSourceErrors session
            if all ((== KindWarning) . errorKind) errors
              then go session mprocessRef
              else loop
          Right RequestGetSourceErrors -> do
            errors <- getSourceErrors session
            send $ ResponseGetSourceErrors errors
            loop
          Right RequestGetLoadedModules -> do
            mods <- getLoadedModules session
            send $ ResponseGetLoadedModules mods
            loop
          Right (RequestGetSpanInfo span) -> do
            case fileMap (spanFilePath span) of
              Just mod -> do
                let mkInfo (span', info) = ResponseSpanInfo info span'
                send $ ResponseGetSpanInfo
                       $ map mkInfo
                       $ spanInfo (moduleName mod) span
              Nothing ->
                send $ ResponseGetSpanInfo []
            loop
          Right (RequestGetExpTypes span) -> do
            send $ ResponseGetExpTypes $
              case fileMap (spanFilePath span) of
                Just mod ->
                  map (\(span', info) -> ResponseExpType info span') $
                  sortSpans $
                  expTypes (moduleName mod) span
                Nothing -> []
            loop
          Right (RequestGetAnnExpTypes span) -> do
            send $ ResponseGetAnnExpTypes $
              case fileMap (spanFilePath span) of
                Just (moduleName -> mn) ->
                  map (annotateTypeInfo (autoComplete mn)) $
                  sortSpans $
                  expTypes mn span
                Nothing -> []
            loop
          Right (RequestGetAutocompletion filePath prefix) -> do
            send $ ResponseGetAutocompletion $
              case fileMap filePath of
                Just mod -> autoComplete (moduleName mod) prefix
                Nothing -> []
            loop
          Right (RequestRun usePty mn identifier) -> do
            actions <- (if usePty then runStmtPty else runStmt)
              session (Text.unpack mn) (Text.unpack identifier)
            writeIORef mprocessRef (Just actions)
            let sendOutput = fix $ \outputLoop -> do
                  result <- runWait actions
                  case result of
                    Left output -> do
                      send $ ResponseProcessOutput (S8.unpack output)
                      outputLoop
                    Right done -> do
                      send $ ResponseProcessDone done
                      writeIORef mprocessRef Nothing
            void $ withAsync sendOutput $ \_ -> loop
          Right (RequestProcessInput input) -> do
            mprocess <- readIORef mprocessRef
            case mprocess of
              Just actions -> supplyStdin actions (S8.pack input)
              Nothing -> send ResponseNoProcessError
            loop
          Right RequestProcessKill -> do
            mprocess <- readIORef mprocessRef
            case mprocess of
              Just actions -> interrupt actions
              Nothing -> send ResponseNoProcessError
            loop
          Right RequestShutdownSession ->
            send $ ResponseShutdownSession
    ignoreStatus :: UpdateStatus -> IO ()
    ignoreStatus _ = return ()

    pullSeq :: Either e (Sequenced a) -> Sequenced (Either e a)
    pullSeq = either (NoSeq . Left) (fmap Right)

annotateTypeInfo :: Autocomplete -> (SourceSpan, Text) -> ResponseAnnExpType
annotateTypeInfo autocomplete (span, info) =
  ResponseAnnExpType (CodeIdInfo <$> annotateType autocomplete info) span

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

makeSessionUpdate :: RequestSessionUpdate -> IdeSessionUpdate
makeSessionUpdate (RequestUpdateTargets targets) =
  updateTargets targets
makeSessionUpdate (RequestSessionUpdate) =
  mempty

-- TODO:
{-makeSessionUpdate (RequestUpdateSourceFile filePath contents) =
  updateSourceFile filePath contents-}
{-makeSessionUpdate (RequestUpdateSourceFileFromFile filePath) =
  updateSourceFileFromFile filePath-}
{-makeSessionUpdate (RequestUpdateSourceFileDelete filePath) =
  updateSourceFileDelete filePath-}
{-makeSessionUpdate (RequestUpdateDataFile filePath contents) =
  updateDataFile filePath contents-}
{-makeSessionUpdate (RequestUpdateDataFileFromFile remoteFile localFile) =
  updateDataFileFromFile remoteFile localFile-}
{-makeSessionUpdate (RequestUpdateDataFileDelete filePath) =
  updateDataFileDelete filePath-}
{-makeSessionUpdate (RequestUpdateGhcOpts options) =
  updateGhcOpts options-}
{-makeSessionUpdate (RequestUpdateRtsOpts options) =
  updateRtsOpts options-}
{-makeSessionUpdate (RequestUpdateEnv variables) =
  updateEnv variables-}
{-makeSessionUpdate (RequestUpdateArgs args) =
  updateArgs args-}

{-makeSessionUpdate (RequestUpdateRelativeIncludes _) = error "FIXME: RequestUpdateRelativeIncludes"-}
{-makeSessionUpdate (RequestUpdateCodeGeneration _) = error "FIXME: RequestUpdateCodeGeneration"-}
