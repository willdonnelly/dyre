module TestDyre ( runDefault, testDyre, Config(..), defaultConf ) where

import qualified Dyre as Dyre
import System
import System.IO

data Config = Config { errorMsg :: Maybe String
                     , message  :: String
                     }

defaultConf = Config { errorMsg = Nothing
                     , message  = "Hello, world!"
                     }

confError :: String -> Config -> Config
confError msg cfg = cfg {errorMsg = Just msg}

realMain :: Config -> IO ()
realMain (Config err msg) = do
    putStrLn "Entered program"
    args <- getArgs
    putStrLn $ "Arguments: " ++ show args
    case err of
         Just eMsg -> putStrLn $ "Error: " ++ eMsg
         Nothing   -> putStrLn $ "Message: " ++ msg

(runDefault, testDyre) = Dyre.runWith Dyre.Params
    { Dyre.projectName  = "testDyre"
    , Dyre.configDir    = return "/home/will/.testDyre"
    , Dyre.tmpDir       = return "/home/will/.testDyre/tmp"
    , Dyre.binDir       = return "/home/will/projects/dyre"
    , Dyre.defaultConf  = defaultConf
    , Dyre.confError    = confError
    , Dyre.realMain     = realMain
    , Dyre.hidePackages = []
    , Dyre.ghcOpts      = []
    , Dyre.statusOut    = hPutStrLn stderr
    }
