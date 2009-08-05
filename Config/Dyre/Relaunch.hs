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
import System.Directory     ( getTemporaryDirectory, removeFile )
import System.Environment   ( getProgName )
import System.Posix.Process ( executeFile, getProcessID )

import System.IO.Storage    ( putValue, delValue )
import Config.Dyre.Options  ( customOptions, getMasterBinary, getStatePersist )

relaunchMaster :: Maybe [String] -> IO ()
relaunchMaster otherArgs = do
    args <- customOptions otherArgs

    -- Get the program name and path
    masterName <- getProgName
    masterPath <- getMasterBinary

    -- If we have a path, run it, otherwise hope the name works
    case masterPath of
         Nothing -> executeFile masterName True args Nothing
         Just mp -> executeFile mp False args Nothing

relaunchWithState :: Show a => a -> Maybe [String] -> IO ()
relaunchWithState state otherArgs = do
    -- Calculate the path to the state file
    progName <- getProgName
    procID   <- getProcessID
    tempDir  <- getTemporaryDirectory
    let statePath = tempDir </> progName ++ "-" ++ (show procID) ++ ".state"

    -- Write the state to the file and store the path
    writeFile statePath (show state)
    putValue "dyre" "persistState" statePath

    -- Relaunch
    relaunchMaster otherArgs

maybeRestoreState :: Read a => IO (Maybe a)
maybeRestoreState = do
    statePath <- getStatePersist
    case statePath of
         Nothing -> return Nothing
         Just sp -> do stateData <- readFile sp
                       removeFile sp
                       delValue "dyre" "persistState"
                       errorToMaybe $ readIO stateData

errorToMaybe :: IO a -> IO (Maybe a)
errorToMaybe a = do tryValue <- try a
                    case tryValue of
                         Left  _ -> return $ Nothing
                         Right v -> return $ Just v

restoreState :: Read a => a -> IO a
restoreState d = fmap (fromMaybe d) maybeRestoreState
