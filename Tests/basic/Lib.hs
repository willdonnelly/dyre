module Lib where

import qualified Config.Dyre as Dyre

import Config.Dyre.Relaunch
import Control.Monad
import System.IO

data Config = Config { message :: String, errorMsg :: Maybe String }

defaultConfig :: Config
defaultConfig = Config "Basic Test Version 1.0" Nothing

showError :: Config -> String -> Config
showError cfg msg = cfg { errorMsg = Just msg }

realMain (Config message (Just err)) = putStrLn "Compile Error"
realMain (Config message Nothing) = do
    state <- restoreTextState 1
    when (state < 3) $ relaunchWithTextState (state + 1) Nothing
    putStrLn $ message ++ " - " ++ show state

basicTest = Dyre.wrapMain $ Dyre.defaultParams
    { Dyre.projectName = "basicTest"
    , Dyre.realMain    = realMain
    , Dyre.showError   = showError
    , Dyre.statusOut   = const . return $ ()
    }
