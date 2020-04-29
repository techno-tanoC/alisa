module Write where

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word
import System.IO (Handle)

class Writable a where
    write :: a -> ByteString -> IO ()

instance Writable Handle where
    write = BS.hPut
