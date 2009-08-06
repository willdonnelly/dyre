{-
This is the only other module aside from 'Config.Dyre' which needs
to be imported specially. It contains functions for restarting the
program (which, usefully, will cause a recompile if the config has
been changed), as well as saving and restoring state across said
restarts.

The impossibly simple function arguments are a consequence of a
little cheating we do using the 'System.IO.Storage' library. Of
course, we can't use the stored data unless something else put
it there, so this module will probably explode horribly if used
outside of a program whose recompilation is managed by Dyre.
-}
module Config.Dyre.Relaunch
  ( relaunchWithState
  , restoreState
  , relaunchMaster
  , maybeRestoreState
  ) where

import Data.Maybe           ( fromMaybe, fromJust )
import System.IO            ( writeFile, readFile )
import System.IO.Error      ( try )
import System.FilePath      ( (</>) )
import System.Directory     ( getTemporaryDirectory, removeFile )

import System.IO.Storage    ( putValue, delValue )
import Config.Dyre.Options  ( customOptions, getMasterBinary, getStatePersist )
import Config.Dyre.Compat   ( customExec, getPIDString )

-- | Just relaunch the master binary. We don't have any important
--   state to worry about. (Or, like when 'relaunchWithState' calls
--   it, we're managing state on our own.)
relaunchMaster :: Maybe [String] -> IO ()
relaunchMaster otherArgs = do
    masterPath <- fmap fromJust getMasterBinary
    customExec masterPath otherArgs

-- | Relaunch the master binary, but first preserve the program
--   state so that we can use the 'restoreState' functions to get
--   it back again later. Since we're not trying to be fancy here,
--   we'll just use 'show' to write it out.
relaunchWithState :: Show a => a -> Maybe [String] -> IO ()
relaunchWithState state otherArgs = do
    -- Calculate the path to the state file
    pidString <- getPIDString
    tempDir   <- getTemporaryDirectory
    let statePath = tempDir </> pidString ++ ".state"
    -- Write the state to the file and store the path
    writeFile statePath . show $ state
    putValue "dyre" "persistState" statePath
    -- Relaunch
    relaunchMaster otherArgs

-- | Restore state that was previously saved by the 'relaunchWithState'
--   function. If unsuccessful, it will simply return Nothing.
maybeRestoreState :: Read a => IO (Maybe a)
-- This could probably be a lot simpler if I grokked
-- monad transformers better.
maybeRestoreState = do
    statePath <- getStatePersist
    case statePath of
         Nothing -> return Nothing
         Just sp -> do
             stateData <- readFile sp
--             removeFile sp
             delValue "dyre" "persistState"
             result <- try $ readIO stateData
             case result of
                  Left  _ -> return $ Nothing
                  Right v -> return $ Just v

-- | Restore state using the 'maybeRestoreState' function, but return
--   the provided default state if we can't get anything better.
restoreState :: Read a => a -> IO a
restoreState d = fmap (fromMaybe d) maybeRestoreState
