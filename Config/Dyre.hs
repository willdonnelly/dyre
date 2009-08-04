module Config.Dyre ( wrapMain, Params(..), defaultParams ) where

import System.IO           ( hPutStrLn, stderr )
import System.Environment  ( getArgs )
import System.Directory    ( doesFileExist )
import System.IO.Storage   ( withStore, getDefaultValue )

import Config.Dyre.Params  ( Params(..) )
import Config.Dyre.Compile ( customCompile )
import Config.Dyre.Exec    ( customExec )
import Config.Dyre.Launch  ( launchMain )
import Config.Dyre.Util    ( storeFlagValue, storeFlagState, getPaths, maybeModTime )

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
wrapMain params@Params{projectName = pName} cfg = withStore "dyre" $ do
    -- Store some data for later
    storeFlagValue "--dyre-state-persist=" "persistState"
    storeFlagValue "--dyre-master-binary=" "masterBinary"
    storeFlagState "--force-reconf"        "forceReconf"
    storeFlagState "--dyre-debug"          "debugMode"

    -- Get the important paths
    (thisBinary, tempBinary, configFile, cacheDir) <- getPaths params

    -- Check their modification times
    thisTime <- maybeModTime thisBinary
    tempTime <- maybeModTime tempBinary
    confTime <- maybeModTime configFile

    -- If there's a config file, and the temp binary is older than something
    -- else, or we were specially told to recompile, then we should recompile.
    let confExists = confTime /= Nothing
    forceReconf <- getDefaultValue "dyre" "forceReconf" False
    errors <- if confExists &&
                 (tempTime < confTime || tempTime < thisTime || forceReconf)
                 then customCompile params
                 else return Nothing

    -- If there's a custom binary and we're not it, run it. Otherwise
    -- just launch the main function.
    customExists <- doesFileExist tempBinary
    if customExists && (thisBinary /= tempBinary)
       then customExec params tempBinary
       else launchMain params errors cfg
