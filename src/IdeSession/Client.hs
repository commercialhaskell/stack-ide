module Main where

import Control.Exception
import Control.Monad
import Data.Aeson (encode)
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
    bracket (initSession optInitParams optConfig) shutdownSession $ \session ->
      forever $ do
        value <- nextInStream input
        case fromJSON value of
         Left err ->
           Lazy.hPut stdout (encode (toJSON (ResponseInvalidRequest err)))
         Right (RequestUpdateSession upd) ->
           putStrLn "Not yet implemented"

startCabalSession :: Options -> CabalOptions -> IO ()
startCabalSession Options{..} CabalOptions{..} = do
  fail "Cabal sessions not yet implemented"
