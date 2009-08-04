module Config.Dyre.Util where

import Data.List                      ( isPrefixOf, (\\) )
import System.Info                    ( os, arch )
import System.FilePath                ( (</>) )
import System.Directory               ( getCurrentDirectory
                                      , doesFileExist
                                      , getModificationTime )
import System.IO.Storage              ( putValue, getValue, getDefaultValue )
import System.Environment             ( getArgs )
import System.Environment.XDG.BaseDir ( getUserCacheDir, getUserConfigDir )
import System.Environment.Executable  ( getExecutablePath )

import Config.Dyre.Params             ( Params(..) )

-- | Extract the value which follows a command line flag, and store
--   it for future reference.
storeFlagValue :: String -> String -> IO ()
storeFlagValue flagString storeName = do
    args <- getArgs
    let flagArg = filter (isPrefixOf flagString) args
    if null flagArg
       then return ()
       else putValue "dyre" storeName $ (head flagArg) \\ flagString

storeFlagState :: String -> String -> IO ()
storeFlagState flagString storeName = do
    args <- getArgs
    putValue "dyre" storeName $ flagString `elem` args

-- | Calculate the paths to the three important files and the
--   cache directory.
getPaths :: Params c -> IO (FilePath, FilePath, FilePath, FilePath)
getPaths params@Params{projectName = pName} = do
    thisBinary <- getExecutablePath
    debugMode  <- getDefaultValue "dyre" "debugMode" False
    cwd <- getCurrentDirectory
    cacheDir  <- case (debugMode, cacheDir params) of
                      (True,  _      ) -> return $ cwd </> "cache"
                      (False, Nothing) -> getUserCacheDir pName
                      (False, Just cd) -> cd
    configDir <- case (debugMode, configDir params) of
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

strippedArgs :: IO [String]
strippedArgs = do
    args <- getArgs
    return $ filterOut args [ "--force-reconf"
                            , "--dyre-debug"
                            , "--dyre-state-persist"
                            , "--dyre-master-binary"
                            ]
  where filterOut xs fs = foldl (\xs f -> filter (not . isPrefixOf f) xs) xs fs
