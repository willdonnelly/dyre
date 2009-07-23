module Config.Dyre.Relaunch
  ( relaunchMaster
  , relaunchWithState
  , maybeRestoreState
  , restoreState
  ) where

import System.IO            ( writeFile )
import System.Directory     ( getTemporaryDirectory )
import System.Environment   ( getProgName, getArgs )
import System.Posix.Process ( executeFile, getProcessID )
import System.IO.Storage    ( getValue, clearAll )
import Data.Maybe           ( fromJust, fromMaybe )
import System.FilePath      ( (</>) )
import System.IO.Error      ( try )

relaunchMaster :: [String] -> IO ()
relaunchMaster args = do
    masterPath <- getValue "dyre" "masterBinary"
    masterName <- getValue "dyre" "programName"
    clearAll "dyre"
    case masterPath of
         Nothing -> executeFile (fromJust masterName) True args Nothing
         Just p  -> executeFile p False args Nothing

relaunchWithState :: Show a => a -> Maybe [String] -> IO ()
relaunchWithState state args = do
    progName <- getProgName
    procID   <- getProcessID
    tempDir  <- getTemporaryDirectory
    let statePath = tempDir </> progName ++ "-" ++ (show procID) ++ ".state"
    writeFile statePath (show state)
    oldArgs <- getArgs
    relaunchMaster $ ("--dyre-state-persist=" ++ statePath):(fromMaybe oldArgs args)

maybeRestoreState :: Read a => IO (Maybe a)
maybeRestoreState = do
    stringData <- getValue "dyre" "persistState"
    case stringData of
         Nothing -> return Nothing
         Just st -> do tryValue <- try $ readIO st
                       case tryValue of
                            Left  _ -> return $ Nothing
                            Right v -> return $ Just v

restoreState :: Read a => a -> IO a
restoreState d = do
    maybeState <- maybeRestoreState
    return . fromMaybe d $ maybeState
