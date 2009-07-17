module Dyre.Params ( Params(..) ) where

data Params cfgType = Params
    { projectName  :: String
    -- ^ The name of the project. This needs to also be the name of
    --   the executablei, and the name of the configuration file.
    , configDir    :: IO FilePath
    -- ^ The directory to look for a configuration file in.
    , tmpDir       :: IO FilePath
    -- ^ The directory to store build files in, including the final
    --   generated executable.
    , binDir       :: IO FilePath
    -- ^ The directory that the 'official' binary will reside in.
    --   This will usually be the same as the 'getBinDir' function
    --   from the 'Paths_<project>' module that Cabal provides.
    , defaultConf  :: cfgType
    -- ^ This is the default configuration which will be used when
    --   the user has not created a custom one.
    , confError    :: String -> cfgType -> cfgType
    -- ^ This function updates the config with an error message if
    --   the new config file is invalid. It is the responsibility
    --   of the program to display this information somehow.
    , realMain     :: cfgType -> IO ()
    -- ^ The main function of the program. It is provided with a
    --   configuration. When the program is unconfigured, it is
    --   given the value of 'defaultConf', otherwise it is given
    --   the modified configuration.
    , hidePackages :: [String]
    -- ^ Packages that need to be hidden during compilation
    , ghcOpts      :: [String]
    -- ^ Miscellaneous GHC compilation settings go here
    , statusOut    :: String -> IO ()
    -- ^ A status output function. Will be called with messages
    --   when Dyre recompiles or launches anything.
    }
