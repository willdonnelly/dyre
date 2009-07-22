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

import Config.Dyre.Params ( Params(..) )
import Data.List          ( isPrefixOf, (\\) )
import System.Environment ( getArgs, withArgs )

-- | Enter the main function, possibly reporting errors and modifying
--   the command-line arguments. This is where control finally gets
--   handed off to the 'realMain' function.
launchMain :: Params cfgType -> Maybe String -> cfgType -> IO ()
launchMain params errors cfg = do
    args <- getArgs
    let newArgs = withArgs . excludeArgs $ args
    case errors of
         Nothing -> newArgs (realMain params $ cfg)
         Just er -> newArgs (realMain params $ (showError params) cfg er)

excludeArgs args = filterOut args [ "--force-reconf"
                                  , "--dyre-debug"
                                  , "--dyre-persist-state"
                                  , "--dyre-master-binary" ]
  where filterOut xs fs = foldl (\xs f -> filter (not . isPrefixOf f) xs) xs fs
