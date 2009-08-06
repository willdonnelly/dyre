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

A full example of using Dyre's recompilation and relaunching features
is as follows:

>-- DyreExample.hs --
>module DyreExample where
>
>import qualified Config.Dyre as Dyre
>
>import Config.Dyre.Relaunch
>import System.IO
>
>data Config = Config { message :: String, errorMsg :: Maybe String }
>data State  = State { bufferLines :: [String] } deriving (Read, Show)
>
>defaultConfig :: Config
>defaultConfig = Config "Dyre Example v0.1" Nothing
>
>showError :: Config -> String -> Config
>showError cfg msg = cfg { errorMsg = Just msg }
>
>realMain Config{message = message, errorMsg = errorMsg } = do
>    (State buffer) <- restoreState $ State []
>    case errorMsg of
>         Nothing -> return ()
>         Just em -> putStrLn $ "Error: " ++ em
>    putStrLn message
>    mapM putStrLn . reverse $ buffer
>    putStr "> " >> hFlush stdout
>    input <- getLine
>    case input of
>         "exit" -> return ()
>         "quit" -> return ()
>         other  -> relaunchWithState (State $ other:buffer) Nothing
>
>dyreExample = Dyre.wrapMain $ Dyre.defaultParams
>    { Dyre.projectName = "dyreExample"
>    , Dyre.realMain    = realMain
>    , Dyre.showError   = showError
>    }

Notice that all of the program logic is contained in the 'DyreExample'
module. The main module of the program is absolutely trivial.

>-- Main.hs --
>module Main where
>import DyreExample
>main = dyreExample defaultConfig

When reading the above program, bear in mind exactly how much of the
code is simply *program logic*. Dyre is designed to intelligently
handle recompilation with a bare minimum of program modification.

Some mention should be made of Dyre's defaults. The 'defaultParams'
structure used in the example defines reasonable default values for
several configuration items. In the absence of any other definitions,
Dyre will default to outputting status messages to stderr, not hiding
any packages during compilation, and passing no special options to GHC.

Also, Dyre will expect configuration files to be placed at the path
'$XDG_CONFIG_HOME/<app>/<app>.hs', and it will store cache files in
the '$XDG_CACHE_HOME/<app>/' directory. The 'System.Environment.XDG'
module will be used to determine these paths, so refer to it for
behaviour on Windows platforms.
-}
module Config.Dyre ( wrapMain, Params(..), defaultParams ) where

import System.IO           ( hPutStrLn, stderr )
import System.Directory    ( doesFileExist )

import Config.Dyre.Params  ( Params(..) )
import Config.Dyre.Compile ( customCompile )
import Config.Dyre.Compat  ( customExec )
import Config.Dyre.Options ( getReconf, getDebug, withDyreOptions )
import Config.Dyre.Paths   ( getPaths, maybeModTime )

-- | A set of reasonable defaults for configuring Dyre. If the minimal set of
--   fields are modified, the program will use the XDG-defined locations for
--   configuration and cache files (see 'System.Environment.XDG.BaseDir' for
--   details), pass no special options to GHC, and will output status messages
--   to stderr.
--
--   The fields that will have to be filled are 'projectName', 'realMain', and
--   'showError'
defaultParams :: Params cfgType
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
    -- Also we don't want to use a custom binary if the conf file is
    -- gone.
    customExists <- doesFileExist tempBinary
    if confExists && customExists && (thisBinary /= tempBinary)
       then do statusOut params $ "Launching custom binary '" ++ tempBinary ++ "'\n"
               customExec tempBinary Nothing
       else realMain params $ case errors of
                                   Nothing -> cfg
                                   Just er -> (showError params) cfg er
