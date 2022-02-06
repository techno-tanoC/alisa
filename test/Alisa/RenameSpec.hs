module Alisa.RenameSpec where

import Alisa.Rename as Rename

import System.FilePath
import System.IO
import System.IO.Temp
import System.Posix
import Test.Hspec

withTempDir = around $ \action -> do
  withSystemTempDirectory "alisa" action

currentRename :: FilePath -> IO Rename
currentRename dir = do
  uid <- getEffectiveUserID
  gid <- getEffectiveGroupID
  Rename.new dir uid gid

spec :: Spec
spec = withTempDir $ do
  let content = "hello"
  let dummy = "dummy"

  describe "copy" $ do
    context "when not exists" $ do
      it "copies to the file" $ \dir -> do
        rename <- currentRename dir

        withSystemTempFile "alisa" $ \path handle -> do
          hPutStr handle content
          hClose handle
          Rename.copy rename path "test" "txt"
          readFile (dir </> "test.txt") `shouldReturn` content

    context "when exists" $ do
      it "copies to the fresh name" $ \dir -> do
        rename <- currentRename dir

        writeFile (dir </> "test.txt") dummy

        withSystemTempFile "alisa" $ \path handle -> do
          hPutStr handle content
          hClose handle
          Rename.copy rename path "test" "txt"
          readFile (dir </> "test(1).txt") `shouldReturn` content

    context "when exists twice" $ do
      it "copies to the fresh name" $ \dir -> do
        rename <- currentRename dir

        writeFile (dir </> "test.txt") dummy
        writeFile (dir </> "test(1).txt") dummy

        withSystemTempFile "alisa" $ \path handle -> do
          hPutStr handle "hello"
          hClose handle
          Rename.copy rename path "test" "txt"
          readFile (dir </> "test(2).txt") `shouldReturn` content
