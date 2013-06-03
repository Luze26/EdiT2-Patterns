module Paths_ediT2 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,5], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/zang/bin"
libdir     = "/home/zang/lib/ediT2-0.5/ghc-7.4.1"
datadir    = "/home/zang/share/ediT2-0.5"
libexecdir = "/home/zang/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "ediT2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ediT2_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ediT2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ediT2_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
