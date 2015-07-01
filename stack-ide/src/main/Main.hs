{-# LANGUAGE CPP #-}

module Main where

import Data.Aeson
import Data.Aeson.Parser
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 (hPutStrLn)
import Stack.Ide
import Stack.Ide.CmdLine
import Stack.Ide.JsonAPI ()
import Stack.Ide.Util.ValueStream (newStream, nextInStream)
import System.IO (stdin, stdout, stderr, hSetBuffering, BufferMode(..))

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
  
  -- Disable buffering for interactive piping
  mapM_ (flip hSetBuffering NoBuffering) [stdout, stderr]
  
  startEmptySession clientIO opts
