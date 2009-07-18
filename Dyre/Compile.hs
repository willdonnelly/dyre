{- |
The functions defined here should never be used directly by a program.
They are documented for readability and maintenance purposes, and their
behaviour should never be relied upon to remain the same. Only the
exports from the main 'Dyre' module should be relied upon.

That said, the functions herein deal with compilation of the custom
configuration. The majority of the code actually deals with error
handling, and not the compilation itself /per se/.
-}
module Dyre.Compile ( customCompile ) where

import System.IO         ( openFile, hClose, IOMode(..) )
import System.Exit       ( ExitCode(..) )
import System.Process    ( runProcess, waitForProcess )
import System.FilePath   ( (</>) )
import System.Directory  ( getCurrentDirectory, createDirectoryIfMissing, doesFileExist, removeFile )
import Control.Exception ( bracket )
import GHC.Paths         ( ghc )

import Dyre.Params

-- | Attempts to compile the configuration file. Errors will be stored in
--   the '<tmpPath>/errors.log' file. Will return a boolean indicating if
--   there is a custom binary to execute.
customCompile :: Params cfgType -> FilePath -> FilePath
              -> FilePath -> IO (Maybe String)
customCompile params@Params{statusOut = output}
              cfgFile tmpFile tmpPath = do
    -- Prepare to compile. Create the temp directory and open the error file.
    output $ "Configuration '" ++ cfgFile ++  "' changed. Recompiling."
    createDirectoryIfMissing True tmpPath
    let errFile = tmpPath </> "errors.log"
    result <- bracket (openFile errFile WriteMode) hClose $ \errHandle -> do
        ghcFlags <- makeFlags params cfgFile tmpFile
        -- We get the GHC path from the one used to compile the main binary
        -- This could be improved.
        ghcProc <- runProcess ghc ghcFlags (Just tmpPath) Nothing
                              Nothing Nothing (Just errHandle)
        waitForProcess ghcProc
    if result /= ExitSuccess
       then output $ "Error occurred while loading configuration file."
       else output $ "Program reconfiguration successful."
    errorsExist <- doesFileExist errFile
    if errorsExist
       then do errors <- readFile errFile
               if errors /= ""
                  then return . Just $ errors
                  else return Nothing
       else return Nothing

-- | Assemble the arguments to GHC so everything compiles right.
makeFlags :: Params cfgType -> FilePath -> FilePath -> IO [String]
makeFlags Params{ghcOpts = flags, hidePackages = hides} cfgFile tmpFile = do
    currentDir <- getCurrentDirectory
    return . concat $ [ ["-v0", "-fforce-recomp", "-i" ++ currentDir]
                      , prefix "-hide-package" hides, flags
                      , ["--make", cfgFile, "-o", tmpFile]
                      ]
  where prefix y xs = concat . map (\x -> [y,x]) $ xs
