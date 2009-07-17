module TestDyre.Boot ( runDefault, runCustom ) where

import qualified Dyre as Dyre

import TestDyre.Main ( realMain, Config(..), defaultConf )

confError :: String -> Config -> Config
confError msg cfg = cfg {errorMsg = Just msg}

(runDefault, runCustom) = Dyre.runWith Dyre.Params
    { Dyre.projectName = "testDyre"
    , Dyre.configDir   = return "/home/will/.testDyre"
    , Dyre.tmpDir      = return "/home/will/.testDyre/tmp"
    , Dyre.binDir      = return "/home/will/projects/dyre"
    , Dyre.defaultConf = defaultConf
    , Dyre.confError   = confError
    , Dyre.realMain    = realMain
    , Dyre.hidePackage = []
    , Dyre.compileOpts = []
    }
