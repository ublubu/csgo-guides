module Paths_server (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/kynan/.cabal/bin"
libdir     = "/home/kynan/.cabal/lib/x86_64-linux-ghc-7.10.2/server-0.1-2R5yVfqVJeIG6X2i81XQFo"
datadir    = "/home/kynan/.cabal/share/x86_64-linux-ghc-7.10.2/server-0.1"
libexecdir = "/home/kynan/.cabal/libexec"
sysconfdir = "/home/kynan/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "server_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "server_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "server_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "server_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "server_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
