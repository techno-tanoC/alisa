module Progress where

import Prelude hiding (Read, read)

import Control.Concurrent.MVar
import Data.Word (Word64)

import Write

newtype Progress a = Progress (MVar (ProgressInner a))

data ProgressInner a = ProgressInner {
    name :: String,
    total :: Word64,
    size :: Word64,
    dest :: a
} deriving (Show, Eq)

new :: String -> a -> IO (Progress a)
new name a = fmap Progress . newMVar $ ProgressInner name 0 0 a

setContentLength :: (Progress a) -> Word64 -> IO ()
setContentLength (Progress pg) cl = modifyMVar_ pg update
    where update inner = return $ inner { total = cl }

progress :: (Progress a) -> Word64 -> IO ()
progress (Progress pg) s = modifyMVar_ pg update
    where update inner = return $ inner { size = size inner + s }
