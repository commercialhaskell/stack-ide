{-# LANGUAGE CPP #-}

module Main where

import Data.Aeson
import Data.Aeson.Parser
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.ByteString.Char8 (hPutStrLn)
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
  -- sophisticated and deals with whitespace properly).
  --
  -- The output is forced before sending to the console.  This is
  -- necessary to avoid protocol errors in circumstances where we
  -- encounter an exception in the input 'Response' value.  In these
  -- cases, we can end up writing a 'ResponseFatalError' in the middle
  -- of a partially serialized 'Response.
  let clientIO = ClientIO
        { sendResponse = hPutStrLn stdout . toStrict . encode . toJSON
        , receiveRequest = fmap fromJSON $ nextInStream input
        }
        where fromJSON = parseEither parseJSON
  opts <- getCommandLineOptions

  -- Disable buffering for interactive piping
  mapM_ (flip hSetBuffering NoBuffering) [stdout, stderr]

  sendExceptions clientIO $ startEmptySession clientIO opts
