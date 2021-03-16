{- |
Compiling the custom executable. The majority of the code actually
deals with error handling, and not the compilation itself /per se/.
-}
module Config.Dyre.Compile ( customCompile, getErrorPath, getErrorString ) where

import Control.Concurrent ( rtsSupportsBoundThreads )
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import System.IO         ( IOMode(WriteMode), withFile )
import System.Environment (lookupEnv)
import System.Exit       ( ExitCode(..) )
import System.Process    ( runProcess, waitForProcess )
import System.FilePath
  ( (</>), dropTrailingPathSeparator, joinPath, splitPath, takeDirectory )
import System.Directory  ( getCurrentDirectory, doesFileExist
                         , createDirectoryIfMissing
                         , renameFile, removeFile )

import Config.Dyre.Paths ( PathsConfig(..), getPathsConfig, outputExecutable )
import Config.Dyre.Params ( Params(..) )

-- | Return the path to the error file.
getErrorPath :: Params cfgType a -> IO FilePath
getErrorPath params =
  (</> "errors.log") . cacheDirectory <$> getPathsConfig params

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
      outFile = outputExecutable tempBinary
      configFile' = configFile paths
      cacheDir' = cacheDirectory paths
      libsDir = libsDirectory paths

    output $ "Configuration '" ++ configFile' ++  "' changed. Recompiling."
    createDirectoryIfMissing True cacheDir'

    -- Compile occurs in here
    errFile <- getErrorPath params
    result <- withFile errFile WriteMode $ \errHandle -> do
        flags <- makeFlags params configFile' outFile cacheDir' libsDir
        stackYaml <- do
          let stackYamlPath = takeDirectory configFile' </> "stack.yaml"
          stackYamlExists <- doesFileExist stackYamlPath
          if stackYamlExists
            then return $ Just stackYamlPath
            else return Nothing

        hc <- fromMaybe "ghc" <$> lookupEnv "HC"
        ghcProc <- maybe (runProcess hc flags (Just cacheDir') Nothing
                              Nothing Nothing (Just errHandle))
                         (\stackYaml' -> runProcess "stack" ("ghc" : "--stack-yaml" : stackYaml' : "--" : flags)
                              Nothing Nothing Nothing Nothing (Just errHandle))
                         stackYaml
        waitForProcess ghcProc

    case result of
      ExitSuccess -> do
        renameFile outFile tempBinary

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
makeFlags params cfgFile outFile cacheDir' libsDir = do
  currentDir <- getCurrentDirectory
  pure . concat $
    [ ["-v0", "-i" ++ libsDir]
    , ["-i" ++ currentDir | includeCurrentDirectory params]
    , prefix "-hide-package" (hidePackages params)

    -- add extra include dirs
    , fmap ("-i" ++) (includeDirs params)

    , includeDirs params >>= getCabalStoreGhcArgs (projectName params)

    , ghcOpts params

    -- if the current process uses threaded RTS,
    -- also compile custom executable with -threaded
    , [ "-threaded" | rtsSupportsBoundThreads ]

    , ["--make", cfgFile, "-outputdir", cacheDir', "-o", outFile]
    , ["-fforce-recomp" | forceRecomp params] -- Only if force is true
    ]
  where prefix y = concatMap $ \x -> [y,x]

-- | Given a path to lib dir, if it is a package in the Cabal
-- store that matches the projectName, return GHC arguments
-- to enable the Cabal store package database and expose the
-- application's library package.
--
getCabalStoreGhcArgs :: String -> FilePath -> [String]
getCabalStoreGhcArgs proj = mkArgs . go . fmap dropTrailingPathSeparator . splitPath
  where
  go :: [String] -> Maybe (String {- unit-id -}, [String] {- package-db -})
  go (".cabal" : "store" : hc : unit : _) =
    case splitOn '-' unit of
      [s, _, _] | s == proj -> Just (unit, [".cabal", "store", hc, "package.db"])
      _                     -> Nothing
  go (h : t@(_cabal : _store : _hc : _unit : _)) = fmap (h:) <$> go t
  go _ = Nothing

  mkArgs Nothing = []
  mkArgs (Just (unitId, pkgDb)) = ["-package-db", joinPath pkgDb, "-package-id", unitId]

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn a l = case span (/= a) l of
  (h, []) -> [h]
  (h, _ : t) -> h : splitOn a t

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists path = do
  exists <- doesFileExist path
  when exists $ removeFile path
