module Paths_ediT2_haskell (
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

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/ediT2-haskell-0.5/ghc-7.4.1"
datadir    = "/usr/local/share/ediT2-haskell-0.5"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "ediT2_haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ediT2_haskell_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ediT2_haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ediT2_haskell_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
