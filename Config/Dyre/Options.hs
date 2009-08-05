module Config.Dyre.Options
  ( getReconf
  , getDebug
  , getMasterBinary
  , getStatePersist
  , withDyreOptions
  , customOptions
  ) where

import Data.List
import Data.Maybe
import System.IO.Storage
import System.Environment
import System.Environment.Executable

-- | Store Dyre's command-line options to the IO-Store "dyre",
--   and then execute the provided IO action with all Dyre's
--   options removed from the arguments.
withDyreOptions action = withStore "dyre" $ do
    args <- getArgs
    storeFlag args "--dyre-state-persist=" "persistState"
    storeFlag args "--dyre-master-binary=" "masterBinary"
    putValue "dyre" "forceReconf"  $ "--force-reconf" `elem` args
    putValue "dyre" "debugMode"    $ "--dyre-debug"   `elem` args
    withArgs (filterArgs args) action
  where filterArgs = filter $ not . prefixElem dyreArgs
        prefixElem xs = or . zipWith ($) (map isPrefixOf xs) . repeat

getReconf :: IO Bool
getReconf = getDefaultValue "dyre" "forceReconf" False

getDebug  :: IO Bool
getDebug = getDefaultValue "dyre" "debugMode" False

getMasterBinary :: IO (Maybe String)
getMasterBinary = getValue "dyre" "masterBinary"

getStatePersist :: IO (Maybe String)
getStatePersist = getValue "dyre" "persistState"

customOptions :: Maybe [String] -> IO [String]
customOptions otherArgs = do
    binaryPath <- getExecutablePath
    masterPath <- getDefaultValue "dyre" "masterBinary" binaryPath
    stateFile  <- getStatePersist
    debugMode  <- getDebug
    return . filter (not . null) $ fromMaybe [] otherArgs ++
        [ if debugMode then "--dyre-debug" else ""
        , case stateFile of
               Nothing -> ""
               Just sf -> "--dyre-state-persist=" ++ sf
        , "--dyre-master-binary=" ++ masterPath
        ]

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
dyreArgs = [ "--force-reconf", "--dyre-state-persist"
           , "--dyre-debug", "--dyre-master-binary" ]
