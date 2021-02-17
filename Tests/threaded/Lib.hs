module Lib where

import Control.Concurrent (rtsSupportsBoundThreads)
import qualified Config.Dyre as Dyre

realMain :: String -> IO ()
realMain s = do
  putStr s *> putChar ' ' *> print rtsSupportsBoundThreads

threadedTest = Dyre.wrapMain
  $ Dyre.newParams "threadedTest" realMain const
