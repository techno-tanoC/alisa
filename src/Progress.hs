module Progress where

import Prelude hiding (Read, read)

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word64)

import Write
import Reporter

newtype Progress a = Progress (MVar (ProgressInner a))

data ProgressInner a = ProgressInner {
    name :: String,
    total :: Word64,
    size :: Word64,
    dest :: a
} deriving (Show, Eq)

instance Writable a => Writable (Progress a) where
    write (Progress m) bs = modifyMVar_ m $ \inner -> do
        let added = size inner + (fromIntegral . BS.length $ bs)
        write (dest inner) bs
        return $ inner { size = added }

new :: String -> a -> IO (Progress a)
new name a = fmap Progress . newMVar $ ProgressInner name 0 0 a

setContentLength :: (Progress a) -> Word64 -> IO ()
setContentLength (Progress pg) cl = modifyMVar_ pg update
    where update inner = return $ inner { total = cl }

toReporter :: (Progress a) -> IO Reporter
toReporter (Progress m) = fmap convert $ readMVar m
    where
        convert (ProgressInner n t s _) = Reporter n t s
