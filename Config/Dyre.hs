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

>    -- DyreExample.hs --
>    module DyreExample where
>
>    import qualified Config.Dyre as Dyre
>    import Config.Dyre.Relaunch
>
>    import System.IO
>
>    data Config = Config { message :: String, errorMsg :: Maybe String }
>    data State  = State { bufferLines :: [String] } deriving (Read, Show)
>
>    defaultConfig :: Config
>    defaultConfig = Config "Dyre Example v0.1" Nothing
>
>    showError :: Config -> String -> Config
>    showError cfg msg = cfg { errorMsg = Just msg }
>
>    realMain Config{message = message, errorMsg = errorMsg } = do
>        (State buffer) <- restoreTextState $ State []
>        case errorMsg of
>             Nothing -> return ()
>             Just em -> putStrLn $ "Error: " ++ em
>        putStrLn message
>        mapM putStrLn . reverse $ buffer
>        putStr "> " >> hFlush stdout
>        input <- getLine
>        case input of
>             "exit" -> return ()
>             "quit" -> return ()
>             other  -> relaunchWithTextState (State $ other:buffer) Nothing
>
>    dyreExample = Dyre.wrapMain $ Dyre.defaultParams
>        { Dyre.projectName = "dyreExample"
>        , Dyre.realMain    = realMain
>        , Dyre.showError   = showError
>        }

Notice that all of the program logic is contained in the 'DyreExample'
module. The main module of the program is absolutely trivial, being
essentially just the default configuration for the program:

>    -- Main.hs --
>    import DyreExample
>    main = dyreExample defaultConfig

The user can then create a custom configuration file, which
overrides some or all of the default configuration:

>    -- ~/.config/dyreExample/dyreExample.hs --
>    import DyreExample
>    main = dyreExample $ defaultConfig { message = "Dyre Example v0.1 (Modified)" }

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

The above example can be tested by running Main.hs with 'runhaskell',
and will detect custom configurations and recompile correctly even when
the library isn't installed, so long as it is in the current directory
when run.
-}
module Config.Dyre ( wrapMain, Params(..), defaultParams ) where

import System.IO           ( hPutStrLn, stderr )
import System.Directory    ( doesFileExist, removeFile, canonicalizePath
                           , getDirectoryContents, doesDirectoryExist )
import System.FilePath     ( (</>) )
import System.Environment  (getArgs)
import GHC.Environment     (getFullArgs)
import Control.Exception   (assert)

import Control.Monad       ( when, filterM )

import Config.Dyre.Params  ( Params(..), RTSOptionHandling(..) )
import Config.Dyre.Compile ( customCompile, getErrorPath, getErrorString )
import Config.Dyre.Compat  ( customExec )
import Config.Dyre.Options ( getForceReconf, getDenyReconf, getDebug
                           , withDyreOptions )
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
    , rtsOptsHandling = RTSAppend []
    , includeCurrentDirectory = True
    }

-- | 'wrapMain' is how Dyre recieves control of the program. It is expected
--   that it will be partially applied with its parameters to yield a 'main'
--   entry point, which will then be called by the 'main' function, as well
--   as by any custom configurations.
wrapMain :: Params cfgType -> cfgType -> IO ()
wrapMain params@Params{projectName = pName} cfg = withDyreOptions params $
    -- Allow the 'configCheck' parameter to disable all of Dyre's recompilation
    -- checks, in favor of simply proceeding ahead to the 'realMain' function.
    if not $ configCheck params
       then realMain params cfg
       else do
        -- Get the important paths
        (thisBinary,tempBinary,configFile,cacheDir,libsDir) <- getPaths params
        libFiles <- recFiles libsDir
        libTimes <- mapM maybeModTime libFiles

        -- Check their modification times
        thisTime <- maybeModTime thisBinary
        tempTime <- maybeModTime tempBinary
        confTime <- maybeModTime configFile
        let confExists = confTime /= Nothing

        denyReconf  <- getDenyReconf
        forceReconf <- getForceReconf
        -- Either the user or timestamps indicate we need to recompile
        let needReconf = or [ tempTime < confTime
                            , tempTime < thisTime
                            , or . map (tempTime <) $ libTimes
                            , forceReconf
                            ]

        -- If we're allowed to reconfigure, a configuration exists, and
        -- we detect a need to recompile it, then go ahead and compile.
        when (not denyReconf && confExists && needReconf)
             (customCompile params)

        -- If there's a custom binary and we're not it, run it. Otherwise
        -- just launch the main function, reporting errors if appropriate.
        -- Also we don't want to use a custom binary if the conf file is
        -- gone.
        errorData    <- getErrorString params
        customExists <- doesFileExist tempBinary

        if confExists && customExists
           then do
               -- Canonicalize the paths for comparison to avoid symlinks
               -- throwing us off. We do it here instead of earlier because
               -- canonicalizePath throws an exception when the file is
               -- nonexistent.
               thisBinary' <- canonicalizePath thisBinary
               tempBinary' <- canonicalizePath tempBinary
               if thisBinary' /= tempBinary'
                  then launchSub errorData tempBinary
                  else enterMain errorData
           else enterMain errorData
  where launchSub errorData tempBinary = do
            statusOut params $ "Launching custom binary " ++ tempBinary ++ "\n"
            givenArgs <- handleRTSOptions $ rtsOptsHandling params
            -- Deny reconfiguration if a compile already failed.
            let arguments = case errorData of
                              Nothing -> givenArgs
                              Just _  -> "--deny-reconf":givenArgs
            -- Execute
            customExec tempBinary $ Just arguments
        enterMain errorData = do
            -- Show the error data if necessary
            let mainConfig = case errorData of
                                  Nothing -> cfg
                                  Just ed -> showError params cfg ed
            -- Enter the main program
            realMain params mainConfig

recFiles :: FilePath -> IO [FilePath]
recFiles d = do
    exists <- doesDirectoryExist d
    if exists
       then do
           nodes <- getDirectoryContents d
           let nodes' = map (d </>) . filter (`notElem` [".", ".."]) $ nodes
           files <- filterM doesFileExist nodes'
           dirs  <- filterM doesDirectoryExist nodes'
           subfiles <- concat `fmap` mapM recFiles dirs
           return $ files ++ subfiles
       else return []

assertM b = assert b $ return ()

-- | Filters GHC runtime system arguments:
filterRTSArgs = filt False
  where
    filt _     []             = []
    filt _     ("--RTS":rest) = []
    filt False ("+RTS" :rest) = filt True  rest
    filt True  ("-RTS" :rest) = filt False rest
    filt False (_      :rest) = filt False rest
    filt True  (arg    :rest) = arg:filt True rest
    --filt state args           = error $ "Error filtering RTS arguments in state " ++ show state ++ " remaining arguments: " ++ show args

editRTSOptions opts (RTSReplace ls) = ls
editRTSOptions opts (RTSAppend ls)  = opts ++ ls

handleRTSOptions h = do fargs <- getFullArgs
                        args  <- getArgs
                        let rtsArgs = editRTSOptions (filterRTSArgs fargs) h
                        assertM $ not $ "--RTS" `elem` rtsArgs
                        case rtsArgs of
                          [] -> if not $ "+RTS" `elem` args
                                  then return args -- cleaner output
                                  else return $ "--RTS":args
                          _  -> return $ ["+RTS"] ++ rtsArgs ++ ["--RTS"] ++ args

