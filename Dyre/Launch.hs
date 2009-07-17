module Dyre.Launch ( launchMain ) where

import Dyre.Params

import System.FilePath      ( (</>) )
import System.Directory     ( doesFileExist, removeFile )

launchMain :: Params cfgType -> Maybe String -> cfgType -> IO ()
launchMain params errors cfg = do
    putStrLn $ "Entering main program"
    case errors of
         Nothing -> realMain params $ cfg
         Just er -> realMain params $ (confError params) er cfg
