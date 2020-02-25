{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_calculus (
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

bindir     = "/Users/rednecked_crake/calculus/.stack-work/install/x86_64-osx/0a0353c6408bba279ec20f174eb84e8c9b204e64029564ea28a6046834fe3a99/8.8.2/bin"
libdir     = "/Users/rednecked_crake/calculus/.stack-work/install/x86_64-osx/0a0353c6408bba279ec20f174eb84e8c9b204e64029564ea28a6046834fe3a99/8.8.2/lib/x86_64-osx-ghc-8.8.2/calculus-0.1.0.0-Jia5F6NKgiAWG1zc1l5ni-calculus-test"
dynlibdir  = "/Users/rednecked_crake/calculus/.stack-work/install/x86_64-osx/0a0353c6408bba279ec20f174eb84e8c9b204e64029564ea28a6046834fe3a99/8.8.2/lib/x86_64-osx-ghc-8.8.2"
datadir    = "/Users/rednecked_crake/calculus/.stack-work/install/x86_64-osx/0a0353c6408bba279ec20f174eb84e8c9b204e64029564ea28a6046834fe3a99/8.8.2/share/x86_64-osx-ghc-8.8.2/calculus-0.1.0.0"
libexecdir = "/Users/rednecked_crake/calculus/.stack-work/install/x86_64-osx/0a0353c6408bba279ec20f174eb84e8c9b204e64029564ea28a6046834fe3a99/8.8.2/libexec/x86_64-osx-ghc-8.8.2/calculus-0.1.0.0"
sysconfdir = "/Users/rednecked_crake/calculus/.stack-work/install/x86_64-osx/0a0353c6408bba279ec20f174eb84e8c9b204e64029564ea28a6046834fe3a99/8.8.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "calculus_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "calculus_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "calculus_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "calculus_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "calculus_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "calculus_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
