{- |
The main module for Dyre. The items that it exports are all that
should ever be needed by a program which uses Dyre. The 'Params'
structure is used for all configuration data, and the 'runWith'
function is used to obtain the program entry points for the given
configuration data.

For example, a basic program might use Dyre in the following way:

>-- DyreExample.hs --
>module DyreExample ( dyreExample, Config(..), defaultConf ) where
>
>import qualified Config.Dyre as Dyre
>import System
>import System.IO
>import System.Directory
>import System.FilePath
>
>data Config = Config { errorMsg :: Maybe String, message  :: String }
>defaultConf = Config { errorMsg = Nothing, message  = "Hello, world!" }
>confError msg cfg = cfg {errorMsg = Just msg}
>
>realMain (Config err msg) = do
>    putStrLn "Entered program"
>    case err of
>         Just eMsg -> putStrLn $ "Error:   " ++ eMsg
>         Nothing   -> putStrLn $ "Message: " ++ msg
>
>dyreExample = Dyre.wrapMain Dyre.Params
>    { Dyre.projectName  = "dyreExample"
>    , Dyre.configDir    = getAppUserDataDirectory "dyreExample"
>    , Dyre.tmpDir       = do configDir <- getAppUserDataDirectory "dyreExample"
>                             return $ configDir </> "tmp"
>    , Dyre.binDir       = getCurrentDirectory
>    -- ^ This only works when the current directory holds the
>    --   binary. In a full project, the Cabal-generated module
>    --   'Paths_<project>' provides a 'getBinDir' function that
>    --   should be used.
>    , Dyre.confError    = confError
>    , Dyre.realMain     = realMain
>    , Dyre.hidePackages = []
>    , Dyre.ghcOpts      = []
>    , Dyre.statusOut    = hPutStrLn stderr
>    }

>-- Main.hs --
>import DyreExample
>main = dyreExample defaultConfig

This will set up a basic project which looks for a configuration file
in ~/.dyreExample, and can recompile and launch it if necessary. The
only major flaw in this snippet is the use of the 'getCurrentDirectory'
function for the binary directory.
-}
module Config.Dyre
    ( wrapMain
    , Params(..)
    , defaultParams
    ) where

import System.IO          ( openFile, IOMode(..), hClose, hPutStrLn, stderr )
import System.Info        ( os, arch )
import System.FilePath    ( (</>) )
import Control.Exception  ( bracket )
import System.Environment ( getArgs )
import System.Directory   ( getModificationTime, doesFileExist,
                            getCurrentDirectory )
import Config.Dyre.Params  ( Params(..) )
import Config.Dyre.Compile ( customCompile )
import Config.Dyre.Exec    ( customExec )
import Config.Dyre.Launch  ( launchMain )

import Data.Maybe ( fromMaybe )

import Data.XDG.BaseDir    ( getUserCacheDir, getUserConfigDir )
import System.Environment.Executable ( getExecutablePath )

defaultParams = Params
    { projectName  = undefined
    , configDir    = Nothing
    , cacheDir     = Nothing
    , realMain     = undefined
    , showError    = undefined
    , hidePackages = []
    , ghcOpts      = []
    , statusOut    = hPutStrLn stderr
    }

-- | 'wrapMain' is how Dyre recieves control of the program. It is expected
--   that it will be partially applied with its parameters to yield a "main"
--   entry point, which will then be called by the 'main' function, as well
--   as by any custom configurations.
wrapMain :: Params cfgType -> cfgType -> IO ()
wrapMain params@Params{projectName = pName} cfg = do
    args <- getArgs
    let debug = "--dyre-debug" `elem` args

    -- Get directories for storing stuff in
    cacheDir  <- if debug
                    then fmap (</> "cache") $ getCurrentDirectory
                    else fromMaybe (getUserCacheDir pName) (cacheDir params)
    configDir <- if debug
                    then getCurrentDirectory
                    else fromMaybe (getUserConfigDir pName) (configDir params)

    -- These are the three important files
    thisBinary <- getExecutablePath
    let tempBinary = cacheDir </> pName ++ "-" ++ os ++ "-" ++ arch
    let configFile = configDir </> pName ++ ".hs"

    -- Check their modification times
    thisTime <- maybeModTime thisBinary
    tempTime <- maybeModTime tempBinary
    confTime <- maybeModTime configFile

    -- If there's a config file, and the temp binary is older than something
    -- else, or we were specially told to recompile, then we should recompile.
    errors <- if and [ confTime /= Nothing
                     , or [ tempTime < confTime
                          , tempTime < thisTime
                          , "--force-reconf" `elem` args ] ]
                 then customCompile params configFile tempBinary cacheDir
                 else return Nothing

    -- If there's a custom binary and we're not it, run it. Otherwise
    -- just launch the main function.
    customExists <- doesFileExist tempBinary
    if customExists && (thisBinary /= tempBinary)
       then customExec params tempBinary
       else launchMain params errors cfg

-- | Check if a file exists. If it exists, return Just the modification
--   time. If it doesn't exist, return Nothing.
maybeModTime path = do
    fileExists <- doesFileExist path
    if fileExists
       then do modTime <- getModificationTime path
               return . Just $ modTime
       else return Nothing
