{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (when)
import Control.Monad.Logger (defaultLogStr, LoggingT(..))
import Data.Aeson
import Data.Aeson.Parser
import Data.Aeson.Types
import Data.ByteString.Char8 (hPutStrLn)
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.Text.Encoding (decodeUtf8)
import Stack.Ide
import Stack.Ide.CmdLine
import Stack.Ide.JsonAPI (Response(ResponseLog))
import Stack.Ide.Util.ValueStream (newStream, nextInStream)
import System.IO (stdin, stdout, stderr, hSetBuffering, BufferMode(..))
import System.Log.FastLogger (fromLogStr)

main :: IO ()
main = do
  opts <- getCommandLineOptions
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
  let sendResponse = hPutStrLn stdout . toStrict . encode . toJSON
      receiveRequest = fmap (parseEither parseJSON) $ nextInStream input
      -- Ideally this wouldn't roundtrip through Utf8 encoding, but ohwell.
      logMessage loc source level str =
        when (optVerbose opts) $
          sendResponse $ ResponseLog $ decodeUtf8 $ fromLogStr $ defaultLogStr loc source level str
      clientIO = ClientIO {..}

  -- Disable buffering for interactive piping
  mapM_ (flip hSetBuffering NoBuffering) [stdout, stderr]

  sendExceptions clientIO $ startEmptySession clientIO opts
