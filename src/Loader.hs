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
run r w =
    when (isSuccess r) $ do
        case contentLength r of
            Just cl -> return ()
            Nothing -> return ()
        load (responseBody r) w

load :: IO ByteString -> Progress a -> IO ()
load r w = doLoad
    where
        doLoad = do
            bs <- r
            print $ BS.length bs
            unless (BS.null bs) $ doLoad

isSuccess :: Response a -> Bool
isSuccess = statusIsSuccessful . responseStatus

contentLength :: Response a -> Maybe Word64
contentLength = (toWord =<<) . lookup hContentLength . fromList . responseHeaders
    where
        toWord = readMaybe . BS.unpack
