{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_rfp_service (
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
version = Version [0,0,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Donna/.cabal/bin"
libdir     = "/Users/Donna/.cabal/lib/x86_64-osx-ghc-8.6.5/rfp-service-0.0.0.1-inplace-rfp-service"
dynlibdir  = "/Users/Donna/.cabal/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/Donna/.cabal/share/x86_64-osx-ghc-8.6.5/rfp-service-0.0.0.1"
libexecdir = "/Users/Donna/.cabal/libexec/x86_64-osx-ghc-8.6.5/rfp-service-0.0.0.1"
sysconfdir = "/Users/Donna/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "rfp_service_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "rfp_service_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "rfp_service_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "rfp_service_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "rfp_service_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "rfp_service_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
