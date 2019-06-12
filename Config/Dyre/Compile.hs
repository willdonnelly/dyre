{- |
Compiling the custom executable. The majority of the code actually
deals with error handling, and not the compilation itself /per se/.
-}
module Config.Dyre.Compile ( customCompile, getErrorPath, getErrorString ) where

import System.IO         ( openFile, hClose, IOMode(..) )
import System.Exit       ( ExitCode(..) )
import System.Process    ( runProcess, waitForProcess )
import System.FilePath   ( (</>), takeDirectory, (<.>), (-<.>))
import System.Directory  ( getCurrentDirectory, doesFileExist
                         , createDirectoryIfMissing
                         , renameFile, removeFile )
import Control.Exception ( bracket )
import Control.Monad     ( when )

import Config.Dyre.Paths  ( getPaths )
import Config.Dyre.Params ( Params(..) )

-- | Return the path to the error file.
getErrorPath :: Params cfgType -> IO FilePath
getErrorPath params = do
    (_,_,_, cacheDir, _) <- getPaths params
    return $ cacheDir </> "errors.log"

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
    (thisBinary, tempBinary, configFile, cacheDir, libsDir) <- getPaths params
    output $ "Configuration '" ++ configFile ++  "' changed. Recompiling."
    createDirectoryIfMissing True cacheDir

    -- Compile occurs in here
    errFile <- getErrorPath params
    result <- bracket (openFile errFile WriteMode) hClose $ \errHandle -> do
        ghcOpts <- makeFlags params configFile tempBinary cacheDir libsDir
        stackYaml <- do
          let stackYamlPath = takeDirectory configFile </> "stack.yaml"
          stackYamlExists <- doesFileExist stackYamlPath
          if stackYamlExists
            then return $ Just stackYamlPath
            else return Nothing
        ghcProc <- maybe (runProcess "ghc" ghcOpts (Just cacheDir) Nothing
                              Nothing Nothing (Just errHandle))
                         (\stackYaml' -> runProcess "stack" ("ghc" : "--stack-yaml" : stackYaml' : "--" : ghcOpts)
                              Nothing Nothing Nothing Nothing (Just errHandle))
                         stackYaml
        waitForProcess ghcProc

    case result of
      ExitSuccess -> do
        renameFile (tempBinary -<.> "tmp") tempBinary

        -- GHC sometimes prints to stderr, even on success.
        -- Other parts of dyre infer error if error file exists
        -- and is non-empty, so remove it.
        removeFile errFile

        output "Program reconfiguration successful."

      _ -> do
        removeFile tempBinary
        output "Error occurred while loading configuration file."

-- | Assemble the arguments to GHC so everything compiles right.
makeFlags :: Params cfgType -> FilePath -> FilePath -> FilePath
          -> FilePath -> IO [String]
makeFlags Params{ghcOpts = flags, hidePackages = hides, forceRecomp = force, includeCurrentDirectory = includeCurDir}
          cfgFile tmpFile cacheDir libsDir = do
    currentDir <- getCurrentDirectory
    return . concat $ [ ["-v0", "-i" ++ libsDir]
                      , if includeCurDir
                          then ["-i" ++ currentDir]
                          else [] 
                      , ["-outputdir", cacheDir]
                      , prefix "-hide-package" hides, flags
                      , ["--make", cfgFile, "-o", tmpFile <.> "tmp"]
                      , ["-fforce-recomp" | force] -- Only if force is true
                      ]
  where prefix y = concatMap $ \x -> [y,x]

