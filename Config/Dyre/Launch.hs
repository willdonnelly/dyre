{- |
The functions defined here should never be used directly by a program.
They are documented for readability and maintenance purposes, and their
behaviour should never be relied upon to remain the same. Only the
exports from the main 'Dyre' module should be relied upon.

That said, this module deals with finally entering the 'realMain'
function. It contains special handling for updating the config
with error data, as well as removing a few special command-line
options that only Dyre needs to see.
-}
module Config.Dyre.Launch ( launchMain ) where

import System.Environment ( withArgs )
import System.IO.Storage  ( clearAll )
import Control.Exception  ( finally )
import Config.Dyre.Params ( Params(..) )
import Config.Dyre.Util   ( strippedArgs )

-- | Enter the main function, possibly reporting errors and modifying
--   the command-line arguments. This is where control finally gets
--   handed off to the 'realMain' function.
launchMain :: Params cfgType -> Maybe String -> cfgType -> IO ()
launchMain params errors cfg = do
    args <- strippedArgs
    let configData = case errors of
                          Nothing -> cfg
                          Just er -> (showError params) cfg er
    finally (withArgs args $ realMain params $ cfg)
            (clearAll "dyre")
