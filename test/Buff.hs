module Buff where

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Write

newtype Buff = Buff (MVar ByteString)

instance Writable Buff where
    write (Buff m) bs = modifyMVar_ m $ \inner -> do
        return $ BS.append inner bs
