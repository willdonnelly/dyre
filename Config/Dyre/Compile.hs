{- |
Compiling the custom executable. The majority of the code actually
deals with error handling, and not the compilation itself /per se/.
-}
module Config.Dyre.Compile ( customCompile, getErrorPath, getErrorString ) where

import System.IO         ( IOMode(..), withFile)
import System.Exit       ( ExitCode(..) )
import System.Process    ( runProcess, waitForProcess )
import System.FilePath   ( (</>), takeDirectory )
import System.Directory  ( getCurrentDirectory, doesFileExist
                         , createDirectoryIfMissing, setCurrentDirectory)

import Config.Dyre.Paths  ( getPaths )
import Config.Dyre.Params ( Params(..) )

import Data.Maybe (isJust)

-- | Return the path to the error file.
getErrorPath :: Params cfgType -> IO FilePath
getErrorPath params = do
    (_,_,_, cacheDirectory, _) <- getPaths params
    return $ cacheDirectory </> "errors.log"

-- | If the error file exists and actually has some contents, return
--   'Just' the error string. Otherwise return 'Nothing'.
getErrorString :: Params cfgType -> IO (Maybe String)
getErrorString params = do
    errorPath   <- getErrorPath params
    errorsExist <- doesFileExist errorPath
    if not errorsExist
       then return Nothing
       else do errorData <- readFile errorPath
               if errorData == ""
                  then return Nothing
                  else return . Just $ errorData

-- | Attempts to compile the configuration file. Will return a string
--   containing any compiler output.
customCompile :: Params cfgType -> IO ()
customCompile params@Params{statusOut = output} = do
    (_, tempBinary, configFile, cacheDirectory, libsDir) <- getPaths params
    output $ "Configuration '" ++ configFile ++  "' changed. Recompiling."
    createDirectoryIfMissing True cacheDirectory

    -- Compile occurs in here
    errFile <- getErrorPath params
    result <- withFile errFile WriteMode $ \errHandle -> do
        ghcOptions <- makeFlags params configFile tempBinary cacheDirectory libsDir
        stackYaml <- do
          let stackYamlPath = takeDirectory configFile </> "stack.yaml"
          stackYamlExists <- doesFileExist stackYamlPath
          if stackYamlExists
            then return $ Just stackYamlPath
            else return Nothing
        output $ "Compiling with " ++ if isJust stackYaml then "stack" else "ghc" ++ "."
        ghcProc <- maybe (runProcess "ghc" ghcOptions (Just cacheDirectory) Nothing Nothing Nothing (Just errHandle))        
                         (\stackYaml' -> runProcess "stack" ("ghc" : "--stack-yaml" : stackYaml' : "--" : ghcOptions) (Just $ takeDirectory configFile) Nothing Nothing Nothing (Just errHandle))
                         stackYaml
        waitForProcess ghcProc

    -- Display a helpful little status message
    if result /= ExitSuccess
       then output "Error occurred while loading configuration file."
       else output "Program reconfiguration successful."

-- | Assemble the arguments to GHC so everything compiles right.
makeFlags :: Params cfgType -> FilePath -> FilePath -> FilePath
          -> FilePath -> IO [String]
makeFlags Params{ghcOpts = flags, hidePackages = hides, forceRecomp = force, includeCurrentDirectory = includeCurDir}
          cfgFile tmpFile cacheDirectory libsDir = do
    currentDir <- getCurrentDirectory
    return . concat $ [ ["-v0", "-i" ++ libsDir]
                      , ["-i" ++ currentDir | includeCurDir]
                      , ["-outputdir", cacheDirectory]
                      , prefix "-hide-package" hides, flags
                      , ["--make", cfgFile, "-o", tmpFile]
                      , ["-fforce-recomp" | force] -- Only if force is true
                      ]
  where prefix y = concatMap $ \x -> [y,x]
