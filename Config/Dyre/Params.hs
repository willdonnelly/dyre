{- |
This module defines the 'Params' datatype which Dyre uses to define
all program-specific configuration data. This module doesn't need to
be imported specially because the 'Params' datatype is re-exported
from the main 'Dyre' module.
-}
module Config.Dyre.Params ( Params(..) ) where

data Params cfgType = Params
    { projectName  :: String
    -- ^ The name of the project. This needs to also be the name of
    --   the executable, and the name of the configuration file.
    , configDir    :: Maybe (IO FilePath)
    -- ^ The directory to look for a configuration file in.
    , cacheDir     :: Maybe (IO FilePath)
    -- ^ The directory to store build files in, including the final
    --   generated executable.
    , realMain     :: cfgType -> IO ()
    -- ^ The main function of the program. When Dyre has completed
    --   all of its recompilation, it passes the configuration data
    --   to this function and gets out of the way.
    , showError    :: cfgType -> String -> cfgType
    -- ^ This function is used to display error messages that occur
    --   during recompilation, by allowing the program to modify its
    --   initial configuration.
    , hidePackages :: [String]
    -- ^ Packages that need to be hidden during compilation
    , ghcOpts      :: [String]
    -- ^ Miscellaneous GHC compilation settings go here
    , statusOut    :: String -> IO ()
    -- ^ A status output function. Will be called with messages
    --   when Dyre recompiles or launches anything. A good value
    --   is 'hPutStrLn stderr', assuming there is no pressing
    --   reason to not put messages on stderr.
    }
