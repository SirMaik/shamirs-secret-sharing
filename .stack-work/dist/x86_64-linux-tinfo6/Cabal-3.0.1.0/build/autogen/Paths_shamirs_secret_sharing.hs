{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_shamirs_secret_sharing (
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

bindir     = "/data/UNAM/Semestre_2021-1/Modelado/Proyectos/Shamirs_Secret_Sharing/shamirs-secret-sharing/.stack-work/install/x86_64-linux-tinfo6/b039ba41412474682e08a8ee0984779952f79972367dfd668144017b6cb89d65/8.8.4/bin"
libdir     = "/data/UNAM/Semestre_2021-1/Modelado/Proyectos/Shamirs_Secret_Sharing/shamirs-secret-sharing/.stack-work/install/x86_64-linux-tinfo6/b039ba41412474682e08a8ee0984779952f79972367dfd668144017b6cb89d65/8.8.4/lib/x86_64-linux-ghc-8.8.4/shamirs-secret-sharing-0.1.0.0-2Rt3L3HXiJHIrbvgr2bDm4"
dynlibdir  = "/data/UNAM/Semestre_2021-1/Modelado/Proyectos/Shamirs_Secret_Sharing/shamirs-secret-sharing/.stack-work/install/x86_64-linux-tinfo6/b039ba41412474682e08a8ee0984779952f79972367dfd668144017b6cb89d65/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/data/UNAM/Semestre_2021-1/Modelado/Proyectos/Shamirs_Secret_Sharing/shamirs-secret-sharing/.stack-work/install/x86_64-linux-tinfo6/b039ba41412474682e08a8ee0984779952f79972367dfd668144017b6cb89d65/8.8.4/share/x86_64-linux-ghc-8.8.4/shamirs-secret-sharing-0.1.0.0"
libexecdir = "/data/UNAM/Semestre_2021-1/Modelado/Proyectos/Shamirs_Secret_Sharing/shamirs-secret-sharing/.stack-work/install/x86_64-linux-tinfo6/b039ba41412474682e08a8ee0984779952f79972367dfd668144017b6cb89d65/8.8.4/libexec/x86_64-linux-ghc-8.8.4/shamirs-secret-sharing-0.1.0.0"
sysconfdir = "/data/UNAM/Semestre_2021-1/Modelado/Proyectos/Shamirs_Secret_Sharing/shamirs-secret-sharing/.stack-work/install/x86_64-linux-tinfo6/b039ba41412474682e08a8ee0984779952f79972367dfd668144017b6cb89d65/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "shamirs_secret_sharing_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "shamirs_secret_sharing_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "shamirs_secret_sharing_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "shamirs_secret_sharing_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "shamirs_secret_sharing_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "shamirs_secret_sharing_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
