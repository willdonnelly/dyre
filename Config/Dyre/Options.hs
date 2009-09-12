{-
Handling for the command-line options that can be used to configure
Dyre. As of the last count, there are four of them, and more are
unlikely to be needed. The only one that a user should ever need to
use is the '--force-reconf' option, so the others all begin with
'--dyre-<option-name>'.

At the start of the program, before anything else occurs, the
'withDyreOptions' function is used to hide Dyre's command-line
options. They are loaded into the IO monad using the module
'System.IO.Storage'. This keeps them safely out of the way of
the user code and our own.

Later, when Dyre needs to access the options, it does so through
the accessor functions defined here. When it comes time to pass
control over to a new binary, it gets an argument list which
preserves the important flags with a call to 'customOptions'.
-}
module Config.Dyre.Options
  ( withDyreOptions
  , customOptions
  , getDenyReconf
  , getForceReconf
  , getDebug
  , getMasterBinary
  , getStatePersist
  ) where

import Data.List
import Data.Maybe
import System.IO.Storage
import System.Environment
import System.Environment.Executable

import Config.Dyre.Params

-- | Store Dyre's command-line options to the IO-Store "dyre",
--   and then execute the provided IO action with all Dyre's
--   options removed from the command-line arguments.
withDyreOptions :: Params c -> IO a -> IO a
withDyreOptions Params{configCheck = check} action = withStore "dyre" $ do
    -- Pretty important
    args <- getArgs

    -- If the flag exists, it overrides the current file. Likewise,
    --   if it doesn't exist, we end up with the path to our current
    --   file. This seems like a sensible way to do it.
    -- Don't use 'getExecutablePath' if we're byassing the rest of Dyre.
    this <- if check then getExecutablePath else getProgName
    putValue "dyre" "masterBinary" this
    storeFlag args "--dyre-master-binary=" "masterBinary"

    -- Load the other important arguments into IO storage.
    storeFlag args "--dyre-state-persist=" "persistState"
    putValue "dyre" "forceReconf"  $ "--force-reconf" `elem` args
    putValue "dyre" "denyReconf"   $ "--deny-reconf"  `elem` args
    putValue "dyre" "debugMode"    $ "--dyre-debug"   `elem` args

    -- We filter the arguments, so now Dyre's arguments 'vanish'
    withArgs (filterArgs args) action
  where filterArgs = filter $ not . prefixElem dyreArgs
        prefixElem xs = or . zipWith ($) (map isPrefixOf xs) . repeat

-- | Get the value of the '--force-reconf' flag, which is used
--   to force a recompile of the custom configuration.
getForceReconf :: IO Bool
getForceReconf = getDefaultValue "dyre" "forceReconf" False

-- | Get the value of the '--deny-reconf' flag, which disables
--   recompilation. This overrides "--force-reconf", too.
getDenyReconf :: IO Bool
getDenyReconf = getDefaultValue "dyre" "denyReconf" False

-- | Get the value of the '--dyre-debug' flag, which is used
--   to debug a program without installation. Specifically,
--   it forces the application to use './cache/' as the cache
--   directory, and './' as the configuration directory.
getDebug  :: IO Bool
getDebug = getDefaultValue "dyre" "debugMode" False

-- | Get the path to the master binary. This is set to the path of
--   the *current* binary unless the '--dyre-master-binary=' flag
--   is set. Obviously, we pass the '--dyre-master-binary=' flag to
--   the custom configured application from the master binary.
getMasterBinary :: IO (Maybe String)
getMasterBinary = getValue "dyre" "masterBinary"

-- | Get the path to a persistent state file. This is set only when
--   the '--dyre-state-persist=' flag is passed to the program. It
--   is used internally by 'Config.Dyre.Relaunch' to save and restore
--   state when relaunching the program.
getStatePersist :: IO (Maybe String)
getStatePersist = getValue "dyre" "persistState"

-- | Return the set of options which will be passed to another instance
--   of Dyre. Preserves the master binary, state file, and debug mode
--   flags, but doesn't pass along the forced-recompile flag. Can be
--   passed a set of other arguments to use, or it defaults to using
--   the current arguments when passed 'Nothing'.
customOptions :: Maybe [String] -> IO [String]
customOptions otherArgs = do
    masterPath <- getMasterBinary
    stateFile  <- getStatePersist
    debugMode  <- getDebug
    mainArgs <- case otherArgs of
                     Nothing -> getArgs
                     Just oa -> return oa
    -- Combine the other arguments with the Dyre-specific ones
    let args = filter (not . null) $
            mainArgs ++ [ if debugMode then "--dyre-debug" else ""
                        , case stateFile of
                               Nothing -> ""
                               Just sf -> "--dyre-state-persist=" ++ sf
                        , "--dyre-master-binary=" ++ fromJust masterPath
                        ]
    return args

-- | Look for the given flag in the argument array, and store
--   its value under the given name if it exists.
storeFlag :: [String] -> String -> String -> IO ()
storeFlag args flag name
    | null match  = return ()
    | otherwise   = putValue "dyre" name $ drop (length flag) (head match)
  where match = filter (isPrefixOf flag) args

-- | The array of all arguments that Dyre recognizes. Used to
--   make sure none of them are visible past 'withDyreOptions'
dyreArgs :: [String]
dyreArgs = [ "--force-reconf", "--deny-reconf"
           , "--dyre-state-persist", "--dyre-debug"
           , "--dyre-master-binary" ]
