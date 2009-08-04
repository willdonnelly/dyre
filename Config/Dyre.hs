module Config.Dyre ( wrapMain, Params(..), defaultParams ) where

import System.IO           ( hPutStrLn, stderr )
import System.Directory    ( doesFileExist )

import Config.Dyre.Params  ( Params(..) )
import Config.Dyre.Compile ( customCompile )
import Config.Dyre.Exec    ( customExec )
import Config.Dyre.Options ( getReconf, getDebug, withDyreOptions )
import Config.Dyre.Paths   ( getPaths, maybeModTime )

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
wrapMain params@Params{projectName = pName} cfg = withDyreOptions $ do
    -- Get the important paths
    (thisBinary, tempBinary, configFile, cacheDir) <- getPaths params

    -- Check their modification times
    thisTime <- maybeModTime thisBinary
    tempTime <- maybeModTime tempBinary
    confTime <- maybeModTime configFile

    -- If there's a config file, and the temp binary is older than something
    -- else, or we were specially told to recompile, then we should recompile.
    let confExists = confTime /= Nothing
    forceReconf <- getReconf
    errors <- if confExists &&
                 (tempTime < confTime || tempTime < thisTime || forceReconf)
                 then customCompile params
                 else return Nothing

    -- If there's a custom binary and we're not it, run it. Otherwise
    -- just launch the main function, reporting errors if appropriate.
    customExists <- doesFileExist tempBinary
    if customExists && (thisBinary /= tempBinary)
       then customExec params tempBinary
       else realMain params $ case errors of
                                   Nothing -> cfg
                                   Just er -> (showError params) cfg er
