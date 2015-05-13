module Paths_haskell_scheme_interpreter (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/pablobcb/.cabal/bin"
libdir     = "/home/pablobcb/.cabal/lib/haskell-scheme-interpreter-0.1.0.0/ghc-7.6.3"
datadir    = "/home/pablobcb/.cabal/share/haskell-scheme-interpreter-0.1.0.0"
libexecdir = "/home/pablobcb/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_scheme_interpreter_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_scheme_interpreter_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "haskell_scheme_interpreter_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_scheme_interpreter_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
