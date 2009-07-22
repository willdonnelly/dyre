module Config.Dyre.Relaunch
  ( relaunchMaster
  , relaunchWithState
  , restoreState
  ) where

import System.IO            ( writeFile )
import System.Directory     ( getTemporaryDirectory )
import System.Environment   ( getProgName )
import System.Posix.Process ( executeFile, getProcessID )
import System.IO.Storage    ( getValue, clearAll )
import Data.Maybe           ( fromJust )
import System.FilePath      ( (</>) )

relaunchMaster :: [String] -> IO ()
relaunchMaster args = do
    masterPath <- getValue "dyre" "masterBinary"
    masterName <- getValue "dyre" "programName"
    clearAll "dyre"
    case masterPath of
         Nothing -> executeFile (fromJust masterName) True args Nothing
         Just p  -> executeFile p False args Nothing

relaunchWithState :: String -> [String] -> IO ()
relaunchWithState stStr args = do
    progName <- getProgName
    procID   <- getProcessID
    tempDir  <- getTemporaryDirectory
    let statePath = tempDir </> progName ++ "-" ++ (show procID) ++ ".state"
    writeFile statePath stStr
    relaunchMaster $ ("--dyre-state-persist=" ++ statePath):args


restoreState :: IO (Maybe String)
restoreState = getValue "dyre" "persistState"
