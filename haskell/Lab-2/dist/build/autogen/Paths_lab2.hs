{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_lab2 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/dimapetrov/.cabal/bin"
libdir     = "/Users/dimapetrov/.cabal/lib/x86_64-osx-ghc-8.2.0.20170704/lab2-0.1.0.0-50gkDaoJtVMFGw9p7kNT7d"
dynlibdir  = "/Users/dimapetrov/.cabal/lib/x86_64-osx-ghc-8.2.0.20170704"
datadir    = "/Users/dimapetrov/.cabal/share/x86_64-osx-ghc-8.2.0.20170704/lab2-0.1.0.0"
libexecdir = "/Users/dimapetrov/.cabal/libexec/x86_64-osx-ghc-8.2.0.20170704/lab2-0.1.0.0"
sysconfdir = "/Users/dimapetrov/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "lab2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "lab2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "lab2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "lab2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lab2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lab2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
