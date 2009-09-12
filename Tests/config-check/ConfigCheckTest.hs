module ConfigCheckTest where

import qualified Config.Dyre as Dyre
import System.IO

configCheckMain = Dyre.wrapMain $ Dyre.defaultParams
    { Dyre.projectName = "configCheckTest"
    , Dyre.realMain    = putStrLn
    , Dyre.showError   = const
    , Dyre.statusOut   = const . return $ ()
    }

configCheckTest = Dyre.wrapMain $ Dyre.defaultParams
    { Dyre.projectName = "configCheckTest"
    , Dyre.realMain    = putStrLn
    , Dyre.showError   = const
    , Dyre.statusOut   = const . return $ ()
    , Dyre.configCheck = False
    }
