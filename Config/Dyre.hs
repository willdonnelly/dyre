{- |
The main module for Dyre. The items that it exports are all that
should ever be needed by a program which uses Dyre. The 'Params'
structure is used for all configuration data, and the 'runWith'
function is used to obtain the program entry points for the given
configuration data.

For example, a basic program might use Dyre in the following way:

>-- DyreExample.hs --
>module DyreExample ( runDefault, dyreExample, Config(..), defaultConf ) where
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
>(runDefault, dyreExample) = Dyre.runWith Dyre.Params
>    { Dyre.projectName  = "dyreExample"
>    , Dyre.configDir    = getAppUserDataDirectory "dyreExample"
>    , Dyre.tmpDir       = do configDir <- getAppUserDataDirectory "dyreExample"
>                             return $ configDir </> "tmp"
>    , Dyre.binDir       = getCurrentDirectory
>    -- ^ This only works when the current directory holds the
>    --   binary. In a full project, the Cabal-generated module
>    --   'Paths_<project>' provides a 'getBinDir' function that
>    --   should be used.
>    , Dyre.defaultConf  = defaultConf
>    , Dyre.confError    = confError
>    , Dyre.realMain     = realMain
>    , Dyre.hidePackages = []
>    , Dyre.ghcOpts      = []
>    , Dyre.statusOut    = hPutStrLn stderr
>    }

>-- Main.hs --
>import DyreExample
>main = runDefault

This will set up a basic project which looks for a configuration file
in ~/.dyreExample, and can recompile and launch it if necessary. The
only major flaw in this snippet is the use of the 'getCurrentDirectory'
function for the binary directory.
-}
module Config.Dyre ( runWith, Params(..) ) where

import System.IO          ( openFile, IOMode(..), hClose )
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


-- | This function returns a tuple of functions: (runDefault, runCustom)
--   These two functions are the entry points of the program. 'runDefault'
--   is where control enters Dyre from the main executable, and 'runCustom'
--   is called by customized executables.
runWith :: Params cfgType -> (IO (), cfgType -> IO ())
runWith params@Params{defaultConf = cfg} =
    ( wrapMain True params cfg, wrapMain False params )

-- | By the time wrapMain gets called, we have all the data we need. Regardless
--   of whether it's the default of custom one, we have a config. We have all
--   the parameters we'll need, and we have an 'orig' argument that tells us
--   whether we're in the original or the custom executable.
wrapMain :: Bool -> Params cfgType -> cfgType -> IO ()
wrapMain orig params@Params{realMain = realMain, projectName = pName} cfg = do
    args <- getArgs
    let debug = "--dyre-debug" `elem` args
    let cwd = getCurrentDirectory

    -- Get all three important paths
    cwd <- getCurrentDirectory
    bPath <- if not debug then binDir params    else return cwd
    tPath <- if not debug then tmpDir params    else return $ cwd </> "tmp"
    cPath <- if not debug then configDir params else return cwd
    -- Calculate the names of the important files
    let binFile = bPath </> pName
    let tmpFile = tPath </> pName ++ "-" ++ os ++ "-" ++ arch
    let cfgFile = cPath </> pName ++ ".hs"
    -- Check their modification times
    binT <- maybeModTime binFile
    tmpT <- maybeModTime tmpFile
    cfgT <- maybeModTime cfgFile

    -- If there's a config file, and the temp binary is older than something
    -- else, then we should recompile.
    errors <- if (cfgT /= Nothing) && or [ tmpT < cfgT
                                         , tmpT < binT
                                         , "--force-reconf" `elem`args ]
                 then customCompile params cfgFile tmpFile tPath
                 else return Nothing

    -- If there's a custom binary and we're not it, run it. Otherwise
    -- just launch the main function.
    customExists <- doesFileExist tmpFile
    if customExists && orig
       then customExec params tmpFile
       else launchMain params errors cfg

-- | Check if a file exists. If it exists, return Just the modification
--   time. If it doesn't exist, return Nothing.
maybeModTime path = do
    fileExists <- doesFileExist path
    if fileExists
       then do modTime <- getModificationTime path
               return . Just $ modTime
       else return Nothing
