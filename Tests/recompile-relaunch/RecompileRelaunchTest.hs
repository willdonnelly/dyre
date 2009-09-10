module RecompileRelaunchTest where

import qualified Config.Dyre as Dyre
import Config.Dyre.Relaunch
import System.IO

realMain message = do
    state <- restoreTextState False
    putStr message >> hFlush stdout
    if state then putStrLn ""
             else relaunchWithTextState True Nothing

recompileRelaunchTest = Dyre.wrapMain $ Dyre.defaultParams
    { Dyre.projectName = "recompileRelaunchTest"
    , Dyre.realMain    = realMain
    , Dyre.showError   = const
    , Dyre.statusOut   = const . return $ ()
    }
