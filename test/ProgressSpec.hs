{-# LANGUAGE OverloadedStrings #-}
module ProgressSpec where

import Test.Hspec

import Buff
import Progress
import Write

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "new" $ do
        it "builds initialized Progress" $ do
            pg <- newProgress "test name" ()
            (n, t, s) <- toTuple pg
            n `shouldBe` "test name"
            t `shouldBe` 0
            s `shouldBe` 0

    describe "setContentLength" $ do
        it "sets Content-Length" $ do
            pg <- newProgress "test name" ()
            setContentLength pg 1000
            (_, t, _) <- toTuple pg
            t `shouldBe` 1000

    describe "write" $ do
        it "progresses size and appends ByteString" $ do
            buf <- newBuff
            pg <- newProgress "test name" buf

            write pg "Hello"
            ProgressInner _ _ s buf <- toInner pg
            toBS buf `shouldReturn` "Hello"
            s `shouldBe` 5

            write pg " World"
            ProgressInner _ _ s buf <- toInner pg
            toBS buf `shouldReturn` "Hello World"
            s `shouldBe` 11
