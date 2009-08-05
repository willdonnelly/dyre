module Config.Dyre.Relaunch
  ( relaunchMaster
  , relaunchWithState
  , maybeRestoreState
  , restoreState
  ) where

import Data.Maybe           ( fromMaybe, fromJust )
import System.IO            ( writeFile, readFile )
import System.IO.Error      ( try )
import System.FilePath      ( (</>) )
import System.Directory     ( getTemporaryDirectory, removeFile )
import System.Posix.Process ( executeFile, getProcessID )

import System.IO.Storage    ( putValue, delValue )
import Config.Dyre.Options  ( customOptions, getMasterBinary, getStatePersist )

relaunchMaster :: Maybe [String] -> IO ()
relaunchMaster otherArgs = do
    args <- customOptions otherArgs
    masterPath <- fmap fromJust getMasterBinary
    executeFile masterPath False args Nothing

relaunchWithState :: Show a => a -> Maybe [String] -> IO ()
relaunchWithState state otherArgs = do
    -- Calculate the path to the state file
    procID   <- getProcessID
    tempDir  <- getTemporaryDirectory
    let statePath = tempDir </> (show procID) ++ ".state"
    -- Write the state to the file and store the path
    writeFile statePath . show $ state
    putValue "dyre" "persistState" statePath
    -- Relaunch
    relaunchMaster otherArgs

-- This could probably be a lot simpler if I grokked
-- monad transformers better.
maybeRestoreState :: Read a => IO (Maybe a)
maybeRestoreState = do
    statePath <- getStatePersist
    case statePath of
         Nothing -> return Nothing
         Just sp -> do
             stateData <- readFile sp
             removeFile sp
             delValue "dyre" "persistState"
             result <- try $ readIO stateData
             case result of
                  Left  _ -> return $ Nothing
                  Right v -> return $ Just v

restoreState :: Read a => a -> IO a
restoreState d = fmap (fromMaybe d) maybeRestoreState
