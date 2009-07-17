import qualified Dyre as Dyre

import System

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
    case err of
         Just eMsg -> putStrLn eMsg
         Nothing   -> putStrLn msg

(runDefault, runCustom) = Dyre.runWith Dyre.Params
    { Dyre.projectName = "testDyre"
    , Dyre.configDir   = return "/home/will/.testDyre"
    , Dyre.tmpDir      = return "/home/will/.testDyre/tmp"
    , Dyre.binDir      = return "/home/will/projects/dyre"
    , Dyre.defaultConf = defaultConf
    , Dyre.confError   = confError
    , Dyre.realMain    = realMain
    }

main = runDefault
