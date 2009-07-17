module Dyre.Exec ( customExec ) where

import Dyre.Params

import System               ( getArgs )
import System.Directory     ( doesFileExist )
import System.Posix.Process ( executeFile )
import Data.List            ( (\\) )

-- | Called at the end of a compilation process, and when the original binary
--   sees a need to hand off control
customExec :: Params cfgType -> FilePath -> IO ()
customExec params@Params{statusOut = output} tmpFile = do
    output $ "Launching custom binary '" ++ tmpFile ++ "'\n"
    args <- getArgs
    executeFile tmpFile False (args \\ ["--force-reconf"]) Nothing
