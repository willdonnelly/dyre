{- |
Compiling the custom executable. The majority of the code actually
deals with error handling, and not the compilation itself /per se/.
-}
module Config.Dyre.Compile ( customCompile, getErrorPath, getErrorString ) where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import System.IO         ( IOMode(WriteMode), withFile )
import System.Environment (lookupEnv)
import System.Exit       ( ExitCode(..) )
import System.Process    ( runProcess, waitForProcess )
import System.FilePath   ( (</>), takeDirectory, (<.>), replaceExtension )
import System.Directory  ( getCurrentDirectory, doesFileExist
                         , createDirectoryIfMissing
                         , renameFile, removeFile )

import Config.Dyre.Paths  ( PathsConfig(..), getPathsConfig )
import Config.Dyre.Params
  ( Params(Params)
  , ghcOpts, hidePackages, forceRecomp, includeCurrentDirectory, statusOut
  )

-- | Return the path to the error file.
getErrorPath :: Params cfgType a -> IO FilePath
getErrorPath params = do
    cacheDir <- cacheDirectory <$> getPathsConfig params
    return $ cacheDir </> "errors.log"

-- | If the error file exists and actually has some contents, return
--   'Just' the error string. Otherwise return 'Nothing'.
getErrorString :: Params cfgType a -> IO (Maybe String)
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
customCompile :: Params cfgType a -> IO ()
customCompile params@Params{statusOut = output} = do
    paths <- getPathsConfig params
    let
      tempBinary = customExecutable paths
      configFile' = configFile paths
      cacheDir = cacheDirectory paths
      libsDir = libsDirectory paths

    output $ "Configuration '" ++ configFile' ++  "' changed. Recompiling."
    createDirectoryIfMissing True cacheDir

    -- Compile occurs in here
    errFile <- getErrorPath params
    result <- withFile errFile WriteMode $ \errHandle -> do
        flags <- makeFlags params configFile' tempBinary cacheDir libsDir
        stackYaml <- do
          let stackYamlPath = takeDirectory configFile' </> "stack.yaml"
          stackYamlExists <- doesFileExist stackYamlPath
          if stackYamlExists
            then return $ Just stackYamlPath
            else return Nothing

        hc <- fromMaybe "ghc" <$> lookupEnv "HC"
        ghcProc <- maybe (runProcess hc flags (Just cacheDir) Nothing
                              Nothing Nothing (Just errHandle))
                         (\stackYaml' -> runProcess "stack" ("ghc" : "--stack-yaml" : stackYaml' : "--" : flags)
                              Nothing Nothing Nothing Nothing (Just errHandle))
                         stackYaml
        waitForProcess ghcProc

    case result of
      ExitSuccess -> do
        renameFile (replaceExtension tempBinary "tmp") tempBinary

        -- GHC sometimes prints to stderr, even on success.
        -- Other parts of dyre infer error if error file exists
        -- and is non-empty, so remove it.
        removeFileIfExists errFile

        output "Program reconfiguration successful."

      _ -> do
        removeFileIfExists tempBinary
        output "Error occurred while loading configuration file."

-- | Assemble the arguments to GHC so everything compiles right.
makeFlags :: Params cfgType a -> FilePath -> FilePath -> FilePath
          -> FilePath -> IO [String]
makeFlags params cfgFile tmpFile cacheDir libsDir = do
  currentDir <- getCurrentDirectory
  pure . concat $
    [ ["-v0", "-i" ++ libsDir]
    , ["-i" ++ currentDir | includeCurrentDirectory params]
    , prefix "-hide-package" (hidePackages params)
    , ghcOpts params
    , ["--make", cfgFile, "-outputdir", cacheDir, "-o", tmpFile <.> "tmp"]
    , ["-fforce-recomp" | forceRecomp params] -- Only if force is true
    ]
  where prefix y = concatMap $ \x -> [y,x]

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists path = do
  exists <- doesFileExist path
  when exists $ removeFile path
