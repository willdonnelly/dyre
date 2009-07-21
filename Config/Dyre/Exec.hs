{- |
The functions defined here should never be used directly by a program.
They are documented for readability and maintenance purposes, and their
behaviour should never be relied upon to remain the same. Only the
exports from the main 'Dyre' module should be relied upon.

That said, the functions defined here handle the messy business of
executing the custom binary on different platforms.
-}
module Config.Dyre.Exec ( customExec ) where

import Config.Dyre.Params

import System.Directory     ( doesFileExist )
import System.Environment   ( getArgs )
import System.Posix.Process ( executeFile )
import Data.List            ( (\\) )

-- | Called when execution needs to be transferred over to
--   the custom-compiled binary.
customExec :: Params cfgType -> FilePath -> IO ()
customExec params@Params{statusOut = output} tmpFile = do
    output $ "Launching custom binary '" ++ tmpFile ++ "'\n"
    args <- getArgs
    executeFile tmpFile False (newArgs args) Nothing
  where newArgs args = "--dyre-custom-child":(args \\ ["--force-reconf"])
