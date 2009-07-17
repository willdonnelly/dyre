module Dyre.Launch ( launchMain ) where

import Dyre.Params
import Data.List          ( isPrefixOf, (\\) )
import System.Environment ( getArgs, withArgs )

launchMain :: Params cfgType -> Maybe String -> cfgType -> IO ()
launchMain params errors cfg = do
    args <- getArgs
    let newArgs = withArgs (args \\ ["--force-reconf", "--dyre-debug"])
    case errors of
         Nothing -> newArgs (realMain params $ cfg)
         Just er -> newArgs (realMain params $ (confError params) er cfg)
