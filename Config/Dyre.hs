{- |
Dyre is a library for configuring your Haskell programs. Like Xmonad,
programs configured with Dyre will look for a configuration file written
in Haskell, which essentially defines a custom program configured exactly
as the user wishes it to be. And since the configuration is written in
Haskell, the user is free to do anything they might wish in the context
of configuring the program.

Dyre places emphasis on elegance of operation and ease of integration
with existing applications. The 'wrapMain' function is the sole entry
point for Dyre. When partially applied with a parameter structure, it
wraps around the 'realMain' value from that structure, yielding an almost
identical function which has been augmented with dynamic recompilation
functionality.

The 'Config.Dyre.Relaunch' module provides the ability to restart the
program (recompiling if applicable), and persist state across restarts,
but it has no impact whatsoever on the rest of the library whether it
is used or not.

A full example of using most of Dyre's major features is as follows:

    -- DyreExample.hs --
    module DyreExample where

    import qualified Config.Dyre as Dyre
    import Config.Dyre.Relaunch

    import System.IO

    data Config = Config { message :: String, errorMsg :: Maybe String }
    data State  = State { bufferLines :: [String] } deriving (Read, Show)

    defaultConfig :: Config
    defaultConfig = Config "Dyre Example v0.1" Nothing

    showError :: Config -> String -> Config
    showError cfg msg = cfg { errorMsg = Just msg }

    realMain Config{message = message, errorMsg = errorMsg } = do
        (State buffer) <- restoreTextState $ State []
        case errorMsg of
             Nothing -> return ()
             Just em -> putStrLn $ "Error: " ++ em
        putStrLn message
        mapM putStrLn . reverse $ buffer
        putStr "> " >> hFlush stdout
        input <- getLine
        case input of
             "exit" -> return ()
             "quit" -> return ()
             other  -> relaunchWithTextState (State $ other:buffer) Nothing

    dyreExample = Dyre.wrapMain $ Dyre.defaultParams
        { Dyre.projectName = "dyreExample"
        , Dyre.realMain    = realMain
        , Dyre.showError   = showError
        }

Notice that all of the program logic is contained in the 'DyreExample'
module. The main module of the program is absolutely trivial, being
essentially just the default configuration for the program:

    -- Main.hs --
    import DyreExample
    main = dyreExample defaultConfig

When reading the above program, notice that the majority of the
code is simply *program logic*. Dyre is designed to intelligently
handle recompilation with a minimum of programmer work.

Some mention should be made of Dyre's defaults. The 'defaultParams'
structure used in the example defines reasonable default values for
most configuration items. The three elements defined above are the
only elements that must be overridden. For documentation of the
parameters, consult the 'Config.Dyre.Params' module.

In the absence of any customization, Dyre will search for configuration
files in '$XDG_CONFIG_HOME/<appName>/<appName>.hs', and will store
cache files in '$XDG_CACHE_HOME/<appName>/' directory. The module
'System.Environment.XDG' is used for this purpose, which also provides
analogous behaviour on Windows.
-}
module Config.Dyre ( wrapMain, Params(..), defaultParams ) where

import System.IO           ( hPutStrLn, stderr )
import System.Directory    ( doesFileExist, removeFile )
import System.Environment  ( getArgs )

import Config.Dyre.Params  ( Params(..) )
import Config.Dyre.Compile ( customCompile, getErrorPath, getErrorString )
import Config.Dyre.Compat  ( customExec )
import Config.Dyre.Options ( getForceReconf, getDenyReconf, getDebug, withDyreOptions )
import Config.Dyre.Paths   ( getPaths, maybeModTime )

-- | A set of reasonable defaults for configuring Dyre. The fields that
--   have to be filled are 'projectName', 'realMain', and 'showError'.
defaultParams :: Params cfgType
defaultParams = Params
    { projectName  = undefined
    , configCheck  = True
    , configDir    = Nothing
    , cacheDir     = Nothing
    , realMain     = undefined
    , showError    = undefined
    , hidePackages = []
    , ghcOpts      = []
    , forceRecomp  = True
    , statusOut    = hPutStrLn stderr
    }

-- | 'wrapMain' is how Dyre recieves control of the program. It is expected
--   that it will be partially applied with its parameters to yield a 'main'
--   entry point, which will then be called by the 'main' function, as well
--   as by any custom configurations.
wrapMain :: Params cfgType -> cfgType -> IO ()
wrapMain params@Params{projectName = pName} cfg = withDyreOptions params $ do
    -- Allow the 'configCheck' parameter to disable all of Dyre's recompilation
    -- checks, in favor of simply proceeding ahead to the 'realMain' function.
    if not $ configCheck params
       then realMain params cfg
       else do
        -- Get the important paths
        (thisBinary, tempBinary, configFile, cacheDir) <- getPaths params

        -- Check their modification times
        thisTime <- maybeModTime thisBinary
        tempTime <- maybeModTime tempBinary
        confTime <- maybeModTime configFile

        let confExists = confTime /= Nothing

        -- If there's a config file, and the temp binary is older than something
        -- else, or we were specially told to recompile, then we should recompile.
        denyReconf  <- getDenyReconf
        forceReconf <- getForceReconf

        if not denyReconf && confExists &&
           (tempTime < confTime || tempTime < thisTime || forceReconf)
           then customCompile params
           else return ()

        -- If there's a custom binary and we're not it, run it. Otherwise
        -- just launch the main function, reporting errors if appropriate.
        -- Also we don't want to use a custom binary if the conf file is
        -- gone.
        errorData    <- getErrorString params
        customExists <- doesFileExist tempBinary
        if confExists && customExists && (thisBinary /= tempBinary)
           then launchSub errorData tempBinary
           else enterMain errorData
  where launchSub errorData tempBinary = do
            statusOut params $ "Launching custom binary " ++ tempBinary ++ "\n"
            -- Deny reconfiguration if a compile already failed.
            arguments <- case errorData of
                Nothing -> getArgs
                Just _  -> ("--deny-reconf":) `fmap` getArgs
            -- Execute
            customExec tempBinary $ Just arguments
        enterMain errorData = do
            -- Show the error data if necessary
            let mainConfig = case errorData of
                                  Nothing -> cfg
                                  Just ed -> showError params cfg ed
            -- Remove the error file if it exists
            errorFile <- getErrorPath params
            errorExists <- doesFileExist errorFile
            if errorExists then removeFile errorFile
                           else return ()
            -- Enter the main program
            realMain params mainConfig
