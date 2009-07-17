module Dyre
  ( Params (..)
  , runWith
  ) where

import System           ( getArgs )
import System.IO        ( hPutStrLn, stderr )
import System.Info      ( compilerName, os, arch )
import System.Time      ( ClockTime )
import System.FilePath  ( (</>) )
import System.Directory ( getModificationTime, doesFileExist )

data Params cfgType = Params
    { projectName :: String
    -- ^ The name of the project. This needs to also be the name of
    --   the executablei, and the name of the configuration file.
    , configDir   :: IO FilePath
    -- ^ The directory to look for a configuration file in.
    , tmpDir      :: IO FilePath
    -- ^ The directory to store build files in, including the final
    --   generated executable.
    , binDir      :: IO FilePath
    -- ^ The directory that the 'official' binary will reside in.
    --   This will usually be the same as the 'getBinDir' function
    --   from the 'Paths_<project>' module that Cabal provides.
    , defaultConf :: cfgType
    -- ^ This is the default configuration which will be used when
    --   the user has not created a custom one.
    , confError   :: String -> cfgType -> cfgType
    -- ^ This function updates the config with an error message if
    --   the new config file is invalid. It is the responsibility
    --   of the program to display this information somehow.
    , realMain    :: cfgType -> IO ()
    -- ^ The main function of the program. It is provided with a
    --   configuration. When the program is unconfigured, it is
    --   given the value of 'defaultConf', otherwise it is given
    --   the modified configuration.
    }

-- | This function returns a tuple of functions: (runDefault, runCustom)
--   These two functions are the entry points of the program. 'runDefault'
--   is where control enters Dyre from the main executable, and 'runCustom'
--   is called by customized executables.
runWith :: Params cfgType -> (IO (), cfgType -> IO ())
runWith params@Params{defaultConf = cfg} =
    ( wrapMain True params cfg, wrapMain False params )

modTime :: FilePath -> IO (Maybe ClockTime)
modTime path =
    do fileExists <- doesFileExist path
       if fileExists
          then getModificationTime path >>= return . Just
          else return Nothing

wrapMain :: Bool -> Params cfgType -> cfgType -> IO ()
wrapMain orig params@Params{realMain = realMain} cfg = do
    -- Do all the path and time stuff
    let pName = projectName params
    bPath <- binDir params
    tPath <- tmpDir params
    cPath <- configDir params
    let binFile = bPath </> pName
    let tmpFile = tPath </> pName ++ "-" ++ os ++ "-" ++ arch
    let cfgFile = cPath </> pName ++ ".hs"
    binT <- modTime binFile
    tmpT <- modTime tmpFile
    cfgT <- modTime cfgFile

    wrapActions (binT, binFile) (tmpT, tmpFile) (cfgT, cfgFile)
  where wrapActions (binT, binFile) (tmpT, tmpFile) (cfgT, cfgFile)
            -- If there's no custom config, proceed to the main function
            | cfgT == Nothing  =  realMain cfg
            -- If the config is newer than the custom binary, recompile it
            | cfgT > tmpT      =  recompile params cfgFile tmpFile
            -- If the official binary is newer than the custom one, recompile
            | binT > tmpT      =  recompile params cfgFile tmpFile
            -- If we're in the original binary, but there's a valid custom one
            | orig             =  execCustom tmpFile
            -- I guess we're clear to go ahead then
            | otherwise        =  realMain cfg

recompile :: Params cfgType -> FilePath -> FilePath -> IO ()
recompile params cfgFile tmpFile = do
    hPutStrLn stderr $ "Configuration '" ++ cfgFile ++  "' changed. Recompiling."
    execCustom tmpFile

execCustom :: FilePath -> IO ()
execCustom tmpFile = do
    hPutStrLn stderr $ "Launching custom binary '" ++ tmpFile ++ "'\n"

-- System.Environment.withArgs
