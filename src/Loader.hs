module Loader where

import Prelude hiding (lookup)

import Control.Monad (unless)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List (find)
import Data.Map.Strict
import Network.HTTP.Client
import Network.HTTP.Types
import Text.Read (readMaybe)

class Write w where
    write :: w -> ByteString -> IO ()

load :: Write w => IO ByteString -> w -> IO ()
load r w = doLoad
    where
        doLoad = do
            bs <- r
            unless (BS.null bs) doLoad

isSuccess :: Response a -> Bool
isSuccess = statusIsSuccessful . responseStatus

contentLength :: Response a -> Maybe Int
contentLength = (toInt =<<) . lookup hContentLength . fromList . responseHeaders
    where
        toInt = readMaybe . BS.unpack
