{- |
Defines the 'Params' datatype which Dyre uses to define all
program-specific configuration data. Shouldn't be imported
directly, as 'Config.Dyre' re-exports it.
-}
module Config.Dyre.Params ( Params(..) ) where

-- | This structure is how all kinds of useful data is fed into Dyre. Of
--   course, only the 'projectName', 'realMain', and 'showError' fields
--   are really necessary. By using the set of default values provided
--   as 'Config.Dyre.defaultParams', you can get all the benefits of
--   using Dyre to configure your program in only five or six lines of
--   code.
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
