{-# LANGUAGE ScopedTypeVariables #-}

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

The functions for saving and loading state come in two variants:
one which uses the 'Read' and 'Show' typeclasses, and one which
uses Data.Binary to serialize it. The 'Read' and 'Show' versions
are much easier to use thanks to automatic deriving, but the
binary versions offer more control over saving and loading, as
well as probably being a bit faster.
-}
module Config.Dyre.Relaunch
  ( relaunchMaster
  , relaunchWithTextState
  , relaunchWithBinaryState
  , saveTextState
  , saveBinaryState
  , restoreTextState
  , restoreBinaryState
  ) where

import Data.Maybe           ( fromMaybe, fromJust )
import System.IO            ( writeFile, readFile )
import Data.Binary          ( Binary, encodeFile, decodeFile )
import Control.Exception    ( try, SomeException )
import System.FilePath      ( (</>) )
import System.Directory     ( getTemporaryDirectory, removeFile )

import System.IO.Storage    ( putValue, delValue )
import Config.Dyre.Options  ( customOptions, getMasterBinary, getStatePersist )
import Config.Dyre.Compat   ( customExec, getPIDString )

-- | Just relaunch the master binary. We don't have any important
--   state to worry about. (Or, like when 'relaunchWith<X>State' calls
--   it, we're managing state on our own). It takes an argument which
--   can optionally specify a new set of arguments. If it is given a
--   value of 'Nothing', the current value of 'getArgs' will be used.
relaunchMaster :: Maybe [String] -> IO ()
relaunchMaster otherArgs = do
    masterPath <- fmap fromJust getMasterBinary
    customExec masterPath otherArgs

-- | Relaunch the master binary, but first preserve the program
--   state so that we can use the 'restoreTextState' functions to
--   get it back again later.
relaunchWithTextState :: Show a => a -> Maybe [String] -> IO ()
relaunchWithTextState state otherArgs = do
    saveTextState state
    relaunchMaster otherArgs

-- | Serialize the state for later restoration with 'restoreBinaryState',
--   and then relaunch the master binary.
relaunchWithBinaryState :: Binary a => a -> Maybe [String] -> IO ()
relaunchWithBinaryState state otherArgs = do
    saveBinaryState state
    relaunchMaster otherArgs

-- | Calculate the path that will be used for saving the state.
--   The path used to load the state, meanwhile, is passed to the
--   program with the '--dyre-persist-state=<path>' flag.
genStatePath :: IO FilePath
genStatePath = do
    pidString <- getPIDString
    tempDir   <- getTemporaryDirectory
    let statePath = tempDir </> pidString ++ ".state"
    putValue "dyre" "persistState" statePath
    return statePath

-- | Serialize a state as text, for later loading with the
--   'restoreTextState' function.
saveTextState :: Show a => a -> IO ()
saveTextState state = do
    statePath <- genStatePath
    writeFile statePath . show $ state

-- | Serialize a state as binary data, for later loading with
--   the 'restoreBinaryState' function.
saveBinaryState :: Binary a => a -> IO ()
saveBinaryState state = do
    statePath <- genStatePath
    encodeFile statePath . Just $ state

-- | Restore state which has been serialized through the
--   'saveTextState' function. Takes a default which is
--   returned if the state doesn't exist.
restoreTextState :: Read a => a -> IO a
restoreTextState d = do
    statePath <- getStatePersist
    case statePath of
         Nothing -> return d
         Just sp -> do
             stateData <- readFile sp
             result <- try $ readIO stateData
             case result of
                  Left  (_ :: SomeException) -> return d
                  Right                    v -> return v

-- | Restore state which has been serialized through the
--   'saveBinaryState' function. Takes a default which is
--   returned if the state doesn't exist.
restoreBinaryState :: Binary a => a -> IO a
restoreBinaryState d = do
    statePath <- getStatePersist
    case statePath of
         Nothing -> return d
         Just sp -> do state <- decodeFile sp
                       return $ fromMaybe d state
