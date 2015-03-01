module Main where

import Control.Exception
import Control.Monad
import Data.Aeson (encode)
import Data.Monoid
import System.IO
import qualified Data.ByteString.Lazy as Lazy

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
startEmptySession Options{..} EmptyOptions{..} = do
    input   <- newStream stdin
    bracket (initSession optInitParams optConfig) shutdownSession $ \session -> do
      updateSession session (updateCodeGeneration True) ignoreProgress
      forever $ do
        value <- nextInStream input
        case fromJSON value of
         Left err ->
           Lazy.hPut stdout (encode (toJSON (ResponseInvalidRequest err)))
         Right (RequestUpdateSession upd) -> do
           updateSession session (mconcat (map makeSessionUpdate upd)) $ \progress ->
             print progress
           putStrLn "Done"
         Right RequestGetSourceErrors -> do
           errors <- getSourceErrors session
           print errors
  where
    ignoreProgress :: Progress -> IO ()
    ignoreProgress _ = return ()

makeSessionUpdate :: RequestSessionUpdate -> IdeSessionUpdate
makeSessionUpdate (RequestUpdateSourceFile filePath contents) =
  updateSourceFile filePath contents
makeSessionUpdate (RequestUpdateSourceFileFromFile filePath) =
  updateSourceFileFromFile filePath
makeSessionUpdate (RequestUpdateGhcOpts options) =
  updateGhcOpts options

startCabalSession :: Options -> CabalOptions -> IO ()
startCabalSession Options{..} CabalOptions{..} = do
  fail "Cabal sessions not yet implemented"
