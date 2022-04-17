{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Cont
import Control.Monad.Trans
import Network.HTTP.Client
import Network.HTTP.Client.TLS

type AppT r m = ContT r m

main :: IO ()
main = flip runContT return $ do
    manager <- newTlsManager
    response <- ContT $ withResponse "https://example.com/" manager
    lift $ print $ responseStatus response
    return ()
