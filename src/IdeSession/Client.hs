module Main where

import Prelude hiding (mod, span)
import Control.Exception
import Data.Monoid
import Data.Text (Text)
import System.IO

import IdeSession
import IdeSession.Client.Cabal
import IdeSession.Client.CmdLine
import IdeSession.Client.JsonAPI
import IdeSession.Client.Util.ValueStream

main :: IO ()
main = do
  opts@Options{..} <- getCommandLineOptions
  case optCommand of
    ShowAPI ->
      putStrLn apiDocs
    StartEmptySession opts' -> do
      putEnc $ ResponseWelcome ideBackendClientVersion
      startEmptySession opts opts'
    StartCabalSession opts' -> do
      putEnc $ ResponseWelcome ideBackendClientVersion
      startCabalSession opts opts'
    ListTargets fp -> do
      putEnc $ ResponseWelcome ideBackendClientVersion
      putEnc =<< listTargets fp

startEmptySession :: Options -> EmptyOptions -> IO ()
startEmptySession Options{..} EmptyOptions{..} =
    bracket (initSession optInitParams optConfig)
            shutdownSession
            mainLoop

startCabalSession :: Options -> CabalOptions -> IO ()
startCabalSession options cabalOptions = do
    bracket (initCabalSession options cabalOptions)
            shutdownSession
            mainLoop

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

type QuerySpanInfo = ModuleName -> SourceSpan -> [(SourceSpan, SpanInfo)]
type QueryExpInfo  = ModuleName -> SourceSpan -> [(SourceSpan, Text)]

mainLoop :: IdeSession -> IO ()
mainLoop session = do
    input <- newStream stdin
    updateSession session (updateCodeGeneration True) ignoreProgress
    go input (\_ _ -> []) (\_ _ -> [])
  where
    -- Main loop
    --
    -- We pass spanInfo and expInfo as argument, which are updated after every
    -- session update (provided that there are no errors). This means that if
    -- the session updates fails we we will the info from the previous update.
    go :: Stream -> QuerySpanInfo -> QueryExpInfo -> IO ()
    go input spanInfo expTypes = do
        value <- nextInStream input
        case fromJSON value of
          Left err -> do
            putEnc $ ResponseInvalidRequest err
            loop
          Right (RequestUpdateSession upd) -> do
            updateSession session (mconcat (map makeSessionUpdate upd)) $ \progress ->
              putEnc $ ResponseUpdateSession (Just progress)
            putEnc $ ResponseUpdateSession Nothing

            errors <- getSourceErrors session
            if null errors
              then do
                spanInfo' <- getSpanInfo session
                expTypes' <- getExpTypes session
                go input spanInfo' expTypes'
              else
                loop
          Right RequestGetSourceErrors -> do
            errors <- getSourceErrors session
            putEnc $ ResponseGetSourceErrors errors
            loop
          Right RequestGetLoadedModules -> do
            mods <- getLoadedModules session
            putEnc $ ResponseGetLoadedModules mods
            loop
          Right (RequestGetSpanInfo mod span) -> do
            let mkInfo (span', info) = ResponseSpanInfo info span'
            spanInfo <- getSpanInfo session
            putEnc $ ResponseGetSpanInfo $ map mkInfo $ spanInfo mod span
            loop
          Right (RequestGetExpTypes mod span) -> do
            let mkInfo (span', info) = ResponseExpType info span'
            putEnc $ ResponseGetExpTypes $ map mkInfo $ expTypes mod span
            loop
          Right RequestShutdownSession ->
            putEnc $ ResponseShutdownSession
      where
        loop = go input spanInfo expTypes

    ignoreProgress :: Progress -> IO ()
    ignoreProgress _ = return ()

makeSessionUpdate :: RequestSessionUpdate -> IdeSessionUpdate
makeSessionUpdate (RequestUpdateSourceFile filePath contents) =
  updateSourceFile filePath contents
makeSessionUpdate (RequestUpdateSourceFileFromFile filePath) =
  updateSourceFileFromFile filePath
makeSessionUpdate (RequestUpdateGhcOpts options) =
  updateGhcOpts options
