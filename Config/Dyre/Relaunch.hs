module Config.Dyre.Relaunch
  ( relaunchMaster
  , relaunchWithState
  , maybeRestoreState
  , restoreState
  ) where

import Data.Maybe           ( fromMaybe )
import System.IO            ( writeFile, readFile )
import System.IO.Error      ( try )
import System.FilePath      ( (</>) )
import System.Directory     ( getTemporaryDirectory )
import System.IO.Storage    ( getValue, getValueDefault, clearAll )
import System.Environment   ( getProgName, getArgs )
import System.Posix.Process ( executeFile, getProcessID )

relaunchMaster :: [String] -> IO ()
relaunchMaster argsA = do
    -- Add debug flag to args if necessary
    debugMode  <- getValueDefault False "dyre" "debugMode"
    let argsB = if debugMode then ("--dyre-debug":argsA) else argsA

    -- Get the path or name of the master program
    masterName <- getProgName
    masterPath <- getValue "dyre" "masterBinary"

    -- Clear the data store
    clearAll "dyre"

    -- If we have a path, run it, otherwise hope the name works
    case masterPath of
         Nothing -> executeFile masterName True argsB Nothing
         Just mp -> executeFile mp False argsB Nothing

relaunchWithState :: Show a => a -> Maybe [String] -> IO ()
relaunchWithState state newArgs = do
    -- Calculate the path to the state file
    progName <- getProgName
    procID   <- getProcessID
    tempDir  <- getTemporaryDirectory
    let statePath = tempDir </> progName ++ "-" ++ (show procID) ++ ".state"

    -- Write the state to the file
    writeFile statePath (show state)

    -- Calculate arguments
    argsA <- getArgs
    let argsB = fromMaybe argsA newArgs
    let argsC = ("--dyre-state-persist=" ++ statePath):argsB

    -- Relaunch
    relaunchMaster argsC

maybeRestoreState :: Read a => IO (Maybe a)
maybeRestoreState = do
    statePath <- getValue "dyre" "persistState"
    case statePath of
         Nothing -> return Nothing
         Just sp -> do stateData <- readFile sp
                       errorToMaybe $ readIO stateData

errorToMaybe :: IO a -> IO (Maybe a)
errorToMaybe a = do tryValue <- try a
                    case tryValue of
                         Left  _ -> return $ Nothing
                         Right v -> return $ Just v

restoreState :: Read a => a -> IO a
restoreState d = fmap (fromMaybe d) maybeRestoreState
