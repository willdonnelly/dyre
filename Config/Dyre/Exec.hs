{- |
The functions defined here should never be used directly by a program.
They are documented for readability and maintenance purposes, and their
behaviour should never be relied upon to remain the same. Only the
exports from the main 'Dyre' module should be relied upon.

That said, the functions defined here handle the messy business of
executing the custom binary on different platforms.
-}
module Config.Dyre.Exec ( customExec ) where

import System.Posix.Process          ( executeFile )
import System.IO.Storage             ( getDefaultValue, getValue )
import System.Environment.Executable ( getExecutablePath )
import Config.Dyre.Util              ( strippedArgs )
import Config.Dyre.Params            ( Params(..) )

-- | Called when execution needs to be transferred over to
--   the custom-compiled binary.
customExec :: Params cfgType -> FilePath -> IO ()
customExec params@Params{statusOut = output} tempBinary = do
    -- Status output
    output $ "Launching custom binary '" ++ tempBinary ++ "'\n"

    -- Calculate some arguments
    binaryPath <- getExecutablePath
    masterPath <- getDefaultValue "dyre" "masterBinary" binaryPath
    stateFile  <- getValue "dyre" "persistState"
    debugMode  <- getDefaultValue "dyre" "debugMode" False
    argsA      <- strippedArgs
    let argsB = if debugMode then ("--dyre-debug":argsA) else argsA
    let argsC = case stateFile of
                     Nothing -> argsB
                     Just sf -> ("--dyre-state-persist=" ++ sf):argsB
    let argsD = ("--dyre-master-binary=" ++ masterPath):argsC

    -- And execute with the new arguments
    executeFile tempBinary False argsD Nothing
