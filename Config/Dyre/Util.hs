module Config.Dyre.Util where

import System.Environment ( getArgs )
import Data.List ( isPrefixOf, (\\) )
import System.IO.Storage ( putValue )
import Config.Dyre.Params ( Params(..) )
import System.Environment.XDG.BaseDir ( getUserCacheDir, getUserConfigDir )
import System.Environment.Executable  ( getExecutablePath )
import System.Directory ( getCurrentDirectory, doesFileExist, getModificationTime )
import System.FilePath ( (</>) )
import System.Info ( os, arch )

-- | Extract the value which follows a command line flag, and store
--   it for future reference.
storeFlagValue :: String -> String -> IO ()
storeFlagValue flagString storeName = do
    args <- getArgs
    let flagArg = filter (isPrefixOf flagString) args
    if null flagArg
       then return ()
       else putValue "dyre" storeName $ (head flagArg) \\ flagString

-- | Calculate the paths to the three important files and the
--   cache directory.
getPaths :: Params c -> IO (FilePath, FilePath, FilePath, FilePath)
getPaths params@Params{projectName = pName} = do
    args <- getArgs
    thisBinary <- getExecutablePath
    let debug = "--dyre-debug" `elem` args
    cwd <- getCurrentDirectory
    cacheDir  <- case (debug, cacheDir params) of
                      (True,  _      ) -> return $ cwd </> "cache"
                      (False, Nothing) -> getUserCacheDir pName
                      (False, Just cd) -> cd
    configDir <- case (debug, configDir params) of
                      (True,  _      ) -> return $ cwd
                      (False, Nothing) -> getUserConfigDir pName
                      (False, Just cd) -> cd
    let tempBinary = cacheDir </> pName ++ "-" ++ os ++ "-" ++ arch
    let configFile = configDir </> pName ++ ".hs"
    return $ (thisBinary, tempBinary, configFile, cacheDir)

-- | Check if a file exists. If it exists, return Just the modification
--   time. If it doesn't exist, return Nothing.
maybeModTime path = do
    fileExists <- doesFileExist path
    if fileExists
       then fmap Just $ getModificationTime path
       else return Nothing
