{- |
Compiling the custom executable. The majority of the code actually
deals with error handling, and not the compilation itself /per se/.
-}
module Config.Dyre.Compile ( customCompile ) where

import System.IO         ( openFile, hClose, IOMode(..) )
import System.Exit       ( ExitCode(..) )
import System.Process    ( runProcess, waitForProcess )
import System.FilePath   ( (</>) )
import System.Directory  ( getCurrentDirectory, doesFileExist
                         , createDirectoryIfMissing )
import Control.Exception ( bracket )
import GHC.Paths         ( ghc )

import Config.Dyre.Paths  ( getPaths )
import Config.Dyre.Params ( Params(..) )

-- | Attempts to compile the configuration file. Will return a string
--   containing any compiler output.
customCompile :: Params cfgType -> IO (Maybe String)
customCompile params@Params{statusOut = output} = do
    (thisBinary, tempBinary, configFile, cacheDir) <- getPaths params
    output $ "Configuration '" ++ configFile ++  "' changed. Recompiling."
    createDirectoryIfMissing True cacheDir

    -- Compile occurs in here
    let errFile = cacheDir </> "errors.log"
    result <- bracket (openFile errFile WriteMode) hClose $ \errHandle -> do
        ghcOpts <- makeFlags params configFile tempBinary cacheDir
        ghcProc <- runProcess ghc ghcOpts (Just cacheDir) Nothing
                              Nothing Nothing (Just errHandle)
        waitForProcess ghcProc

    -- Display a helpful little status message
    if result /= ExitSuccess
       then output $ "Error occurred while loading configuration file."
       else output $ "Program reconfiguration successful."

    -- If the error file exists and actually has some contents, return
    -- 'Just' the error string. Otherwise return 'Nothing'.
    errorsExist <- doesFileExist errFile
    if not errorsExist
       then return Nothing
       else do errors <- readFile errFile
               if errors == ""
                  then return Nothing
                  else return . Just $ errors

-- | Assemble the arguments to GHC so everything compiles right.
makeFlags :: Params cfgType -> FilePath -> FilePath -> FilePath -> IO [String]
makeFlags Params{ghcOpts = flags, hidePackages = hides}
          cfgFile tmpFile cacheDir = do
    currentDir <- getCurrentDirectory
    return . concat $ [ ["-v0", "-fforce-recomp", "-i" ++ currentDir]
                      , ["-odir", cacheDir, "-hidir", cacheDir]
                      , prefix "-hide-package" hides, flags
                      , ["--make", cfgFile, "-o", tmpFile]
                      ]
  where prefix y xs = concat . map (\x -> [y,x]) $ xs
