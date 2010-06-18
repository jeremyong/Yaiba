module Paths_Yaiba (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,2,2], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/jeremy/.cabal/bin"
libdir     = "/Users/jeremy/.cabal/lib/Yaiba-0.2.2/ghc-6.12.3"
datadir    = "/Users/jeremy/.cabal/share/Yaiba-0.2.2"
libexecdir = "/Users/jeremy/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "Yaiba_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "Yaiba_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "Yaiba_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "Yaiba_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
