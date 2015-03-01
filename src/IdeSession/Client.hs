module Main where

import Prelude hiding (mod, span)
import Control.Exception
import Data.Aeson (encode)
import Data.Monoid
import Data.Text (Text)
import Language.JsonGrammar (Json)
import System.IO
import qualified Data.ByteString.Lazy.Char8 as Lazy

import IdeSession.Client.JsonAPI
import IdeSession.Client.CmdLine
import IdeSession.Client.Util.ValueStream
import IdeSession

main :: IO ()
main = do
  opts@Options{..} <- getCommandLineOptions
  case optCommand of
    ShowAPI ->
      putStrLn apiDocs
    StartEmptySession opts' ->
      startEmptySession opts opts'
    StartCabalSession opts' ->
      startCabalSession opts opts'

startEmptySession :: Options -> EmptyOptions -> IO ()
startEmptySession Options{..} EmptyOptions{..} =
    bracket (initSession optInitParams optConfig)
            shutdownSession
            mainLoop

startCabalSession :: Options -> CabalOptions -> IO ()
startCabalSession Options{..} CabalOptions{..} = do
  fail "Cabal sessions not yet implemented"

-- | Version of the client API
--
-- This should be incremented whenever we make a change that editors might need
-- to know about.
ideBackendClientVersion :: VersionInfo
ideBackendClientVersion = VersionInfo {
      versionInfoClient = 1
    }

{-------------------------------------------------------------------------------
  Main loop

  Assumes the session has been properly initialized
-------------------------------------------------------------------------------}

type QuerySpanInfo = ModuleName -> SourceSpan -> [(SourceSpan, SpanInfo)]
type QueryExpInfo  = ModuleName -> SourceSpan -> [(SourceSpan, Text)]

mainLoop :: IdeSession -> IO ()
mainLoop session = do
    input <- newStream stdin
    putEnc $ ResponseWelcome ideBackendClientVersion
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
          Right (RequestGetSpanInfo mod span) -> do
            putEnc $ ResponseGetSpanInfo (spanInfo mod span)
            loop
          Right (RequestGetExpTypes mod span) -> do
            putEnc $ ResponseGetExpTypes (expTypes mod span)
            loop
          Right RequestShutdownSession ->
            putEnc $ ResponseShutdownSession
      where
        loop = go input spanInfo expTypes

    ignoreProgress :: Progress -> IO ()
    ignoreProgress _ = return ()

putEnc :: Json a => a -> IO ()
putEnc = Lazy.hPutStrLn stdout . encode . toJSON

makeSessionUpdate :: RequestSessionUpdate -> IdeSessionUpdate
makeSessionUpdate (RequestUpdateSourceFile filePath contents) =
  updateSourceFile filePath contents
makeSessionUpdate (RequestUpdateSourceFileFromFile filePath) =
  updateSourceFileFromFile filePath
makeSessionUpdate (RequestUpdateGhcOpts options) =
  updateGhcOpts options
