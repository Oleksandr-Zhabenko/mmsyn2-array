{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_mmsyn2_array (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [0,3,1,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/oleksandr/Haskell/mmsyn2-array/.stack-work/install/x86_64-linux-nix/c6c41ca7c6f7663999006c09cf60d21244090d808d387381b3580ae07f4af8aa/9.2.8/bin"
libdir     = "/home/oleksandr/Haskell/mmsyn2-array/.stack-work/install/x86_64-linux-nix/c6c41ca7c6f7663999006c09cf60d21244090d808d387381b3580ae07f4af8aa/9.2.8/lib/x86_64-linux-ghc-9.2.8/mmsyn2-array-0.3.1.1-3bMBfz9u5ZQJSwis3DYXuT"
dynlibdir  = "/home/oleksandr/Haskell/mmsyn2-array/.stack-work/install/x86_64-linux-nix/c6c41ca7c6f7663999006c09cf60d21244090d808d387381b3580ae07f4af8aa/9.2.8/lib/x86_64-linux-ghc-9.2.8"
datadir    = "/home/oleksandr/Haskell/mmsyn2-array/.stack-work/install/x86_64-linux-nix/c6c41ca7c6f7663999006c09cf60d21244090d808d387381b3580ae07f4af8aa/9.2.8/share/x86_64-linux-ghc-9.2.8/mmsyn2-array-0.3.1.1"
libexecdir = "/home/oleksandr/Haskell/mmsyn2-array/.stack-work/install/x86_64-linux-nix/c6c41ca7c6f7663999006c09cf60d21244090d808d387381b3580ae07f4af8aa/9.2.8/libexec/x86_64-linux-ghc-9.2.8/mmsyn2-array-0.3.1.1"
sysconfdir = "/home/oleksandr/Haskell/mmsyn2-array/.stack-work/install/x86_64-linux-nix/c6c41ca7c6f7663999006c09cf60d21244090d808d387381b3580ae07f4af8aa/9.2.8/etc"

getBinDir     = catchIO (getEnv "mmsyn2_array_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "mmsyn2_array_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "mmsyn2_array_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "mmsyn2_array_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mmsyn2_array_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mmsyn2_array_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
