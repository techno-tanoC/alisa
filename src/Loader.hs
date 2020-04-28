module Loader where

import Prelude hiding (lookup)

import Control.Monad (when, unless)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List (find)
import Data.Map.Strict
import Data.Word (Word64)
import Network.HTTP.Client
import Network.HTTP.Types
import Text.Read (readMaybe)

import Progress

run :: Response BodyReader -> Progress a -> IO ()
run res pg =
    when (isSuccess res) $ do
        case contentLength res of
            Just cl -> setContentLength pg cl
            Nothing -> return ()
        load (responseBody res) pg

load :: IO ByteString -> Progress a -> IO ()
load r w = go
    where
        go = do
            bs <- r
            print $ BS.length bs
            unless (BS.null bs) go

isSuccess :: Response a -> Bool
isSuccess = statusIsSuccessful . responseStatus

contentLength :: Response a -> Maybe Word64
contentLength = (toWord =<<) . lookup hContentLength . fromList . responseHeaders
    where
        toWord = readMaybe . BS.unpack
