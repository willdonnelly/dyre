module Config.Dyre.Paths where

import System.Info                    (os, arch)
import System.FilePath                ( (</>), (<.>), takeExtension )
import System.Directory               (getCurrentDirectory, doesFileExist, getModificationTime)
import System.Environment.XDG.BaseDir (getUserCacheDir, getUserConfigDir)
import System.Environment             (getExecutablePath)
import Data.Time

import Config.Dyre.Params
import Config.Dyre.Options

-- | Return the paths to, respectively, the current binary, the custom
--   binary, the config file, and the cache directory.
getPaths :: Params c -> IO (FilePath, FilePath, FilePath, FilePath, FilePath)
getPaths params@Params{projectName = pName} = do
    thisBinary <- getExecutablePath
    debugMode  <- getDebug
    cwd <- getCurrentDirectory
    cacheDir  <- case (debugMode, cacheDir params) of
                      (True,  _      ) -> return $ cwd </> "cache"
                      (False, Nothing) -> getUserCacheDir pName
                      (False, Just cd) -> cd
    configDir <- case (debugMode, configDir params) of
                      (True,  _      ) -> return cwd
                      (False, Nothing) -> getUserConfigDir pName
                      (False, Just cd) -> cd
    let tempBinary = cacheDir </> pName ++ "-" ++ os ++ "-" ++ arch
                              <.> takeExtension thisBinary
    let configFile = configDir </> pName ++ ".hs"
    let libsDir = configDir </> "lib"
    return (thisBinary, tempBinary, configFile, cacheDir, libsDir)

-- | Check if a file exists. If it exists, return Just the modification
--   time. If it doesn't exist, return Nothing.
maybeModTime :: FilePath -> IO (Maybe UTCTime)
maybeModTime path = do
    fileExists <- doesFileExist path
    if fileExists
       then fmap Just $ getModificationTime path
       else return Nothing
