{-# LANGUAGE CPP #-}

module Main where

import Data.Aeson
import Data.Aeson.Parser
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 (hPutStrLn)
import IdeSession.Client
import IdeSession.Client.CmdLine
import IdeSession.Client.JsonAPI ()
import IdeSession.Client.Util.ValueStream (newStream, nextInStream)
import System.IO (stdin, stdout)

main :: IO ()
main = do
  input <- newStream stdin
  -- We separate JSON values in the output by newlines, so that
  -- editors have a means to split the input into separate
  -- values. (The parser on the Haskell side is a lot more
  -- sophisticated and deals with whitespace properly)
  let clientIO = ClientIO
        { sendResponse = hPutStrLn stdout . encode . toJSON
        , receiveRequest = fmap fromJSON $ nextInStream input
        }
        where fromJSON = parseEither parseJSON
  opts <- getCommandLineOptions
  case optCommand opts of
    ShowAPI -> putStrLn "TODO: API"
    StartEmptySession opts' -> startEmptySession clientIO opts opts'
