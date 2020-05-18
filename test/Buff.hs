module Buff where

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Write

newtype Buff = Buff (MVar ByteString)

instance Writable Buff where
    write (Buff m) bs = modifyMVar_ m $ \inner -> do
        return $ BS.append inner bs

newBuff :: IO Buff
newBuff = fmap Buff $ newMVar BS.empty

toBS :: Buff -> IO ByteString
toBS (Buff m) = readMVar m
