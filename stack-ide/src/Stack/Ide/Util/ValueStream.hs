-- | Simple stream wrapper around attoparsec
module Stack.Ide.Util.ValueStream (
    Stream
  , newStream
  , nextInStream
  ) where

import Data.Aeson (Value)
import Data.IORef
import System.IO
import qualified Control.Exception             as Ex
import qualified Data.Aeson.Parser             as Aeson (json)
import qualified Data.Attoparsec.ByteString    as Att
import qualified Data.ByteString               as Strict
import qualified Data.ByteString.Lazy.Internal as Lazy

data Stream = Stream Handle (IORef Strict.ByteString)

newStream :: Handle -> IO Stream
newStream h = do
  st <- newIORef $ Strict.empty
  return $ Stream h st

nextInStream :: Stream -> IO Value
nextInStream (Stream h st) = readIORef st >>= go . Att.parse Aeson.json
  where
    go :: Att.Result Value -> IO Value
    go (Att.Fail _ _ err) =
      Ex.throwIO (userError err)
    go (Att.Partial k) = do
      mchunk <- Ex.try $ Strict.hGetSome h Lazy.defaultChunkSize
      case mchunk of
        Left  ex    -> Ex.throwIO (ex :: Ex.SomeException)
        Right chunk -> go $ k chunk
    go (Att.Done leftover value) = do
      writeIORef st leftover
      return value
