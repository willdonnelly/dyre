module BasicTest where

import qualified Config.Dyre as Dyre

import Config.Dyre.Relaunch
import System.IO

data Config = Config { message :: String, errorMsg :: Maybe String }

defaultConfig :: Config
defaultConfig = Config "Basic Test Version 1.0" Nothing

showError :: Config -> String -> Config
showError cfg msg = cfg { errorMsg = Just msg }

realMain Config{message = message, errorMsg = errorMsg } = do
    case errorMsg of
         Just em -> putStrLn "Compile Error"
         Nothing -> do
            state <- restoreTextState 1
            if state < 3
               then relaunchWithTextState (state + 1) Nothing
               else return ()
            putStrLn $ message ++ " - " ++ show state

basicTest = Dyre.wrapMain $ Dyre.defaultParams
    { Dyre.projectName = "basicTest"
    , Dyre.realMain    = realMain
    , Dyre.showError   = showError
    , Dyre.statusOut   = const . return $ ()
    }
