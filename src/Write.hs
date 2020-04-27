module Write where

import Data.ByteString
import Data.Word

class Writable a where
    write :: a -> ByteString -> IO ()

data Cursor = Cursor {
    pos :: Int,
    content :: ByteString
}
