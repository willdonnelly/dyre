module TestDyre.Main ( realMain, Config(..), defaultConf ) where

import System

data Config = Config { errorMsg :: Maybe String
                     , message  :: String
                     }

defaultConf = Config { errorMsg = Nothing
                     , message  = "Hello, world!"
                     }

realMain :: Config -> IO ()
realMain (Config err msg) = do
    putStrLn "Entered program"
    case err of
         Just eMsg -> putStrLn $ "Error: " ++ eMsg
         Nothing   -> putStrLn $ "Message: " ++ msg
