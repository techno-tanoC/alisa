module Alisa.Rename where

import Control.Concurrent
import System.Directory
import System.FilePath
import System.Posix

type Name = String
type Ext = String

data Rename = Rename
  { lock :: MVar ()
  , root :: FilePath
  , uid :: UserID
  , gid :: GroupID
  }

new :: FilePath -> UserID -> GroupID -> IO Rename
new root uid gid = do
  lock <- newMVar ()
  return $ Rename lock root uid gid

copy :: Rename -> FilePath -> Name -> Ext -> IO ()
copy (Rename lock root uid gid) src n e = withMVar lock $ \() -> do
  fresh <- freshName root (sanitize n) e
  copyFile src fresh
  setOwnerAndGroup fresh uid gid

freshName :: FilePath -> Name -> Ext -> IO FilePath
freshName p n e = go 0
  where
    fullPath i = p </> buildName n i e
    go i = do
      b <- doesPathExist $ fullPath i
      if b then
        go (i + 1)
      else
        return $ fullPath i

buildName :: String -> Int -> String -> FilePath
buildName n i e = n ++ count i ++ tail e
  where
    count :: Int -> String
    count x | x <= 0 = ""
            | otherwise = "(" ++ show x ++ ")"
    tail :: String -> String
    tail "" = ""
    tail ext = '.' : ext

sanitize :: FilePath -> FilePath
sanitize [] = []
sanitize ('/' : cs) = '／' : sanitize cs
sanitize (c : cs) = c : sanitize cs
