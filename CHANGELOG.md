# 0.9.0

- `realMain` can now **return arbitrary types**.  To support this
  change, `Params` got a new type variable.

  ```haskell
  -- before
  data Params cfgType
  wrapMain :: Params cfgType -> cfgType -> IO ()

  -- after
  data Params cfgType a
  wrapMain :: Params cfgType a -> cfgType -> IO a
  ```

- `defaultParams`, which contains `undefined` fields, has been
  **deprecated** in favour of the new function `newParams`:

  ```haskell
  -- here be bottoms
  defaultParams :: Params cfg a

  -- celestial music playing
  newParams
    :: String                 -- ^ 'projectName'
    -> (cfg -> IO a)          -- ^ 'realMain' function
    -> (cfg -> String -> cfg) -- ^ 'showError' function
    -> Params cfg a
  ```

  `newParams` takes values for the three required fields, so program
  authors can clearly see what they have to do and are less likely
  to make a mistake.

- **Cabal store support**: Users can add extra include dirs via the
  `includeDirs` field of `Params`.  The program author just has to
  put the package's library directory in the new `includeDirs`
  field:

  ```haskell
  import Config.Dyre
  import Paths_myapp (getLibDir)

  realMain  = …
  showError = …

  myapp cfg = do
    libdir <- getLibDir
    let params = (newParams "myapp" realMain showError)
          { includeDirs = [libdir] }
    wrapMain params cfg
  ```

  If an include dir appears to be in a Cabal store and matches the
  `projectName`, Dyre adds the corresponding `-package-id` option.
  As a result, recompilation works for programs installed via `cabal
  install`.

- **Stack support**: if Dyre detects a `stack.yaml` alongside the
  custom configuration, it will use Stack to compile the program.
  Credit to *Jaro Reinders* for this feature.

- Dyre compiles the custom executable with **`-threaded`** when the
  main executable uses the threaded RTS.  This means one less thing
  for program authors to remember (or forget) to do.

- Dyre now **requires GHC >= 7.10**.

- Improved **documentation**.

- The **test suite** was expanded, and can now be executed via
  `cabal test`.

- Dyre **cleans up** better after compilation (successful or
  unsuccesful), and behaves better when the custom configuration is
  removed.

- Some versions of GHC write to standard error, even during a
  successful compilation.  Dyre no longer treats this as a
  compilation failure, instead relying solely on GHC's exit status.

- Dyre recognises the **`HC` environment variable**.  If set, it
  will compile the program using the specified compiler.

- Fixes for **Windows**, including working with recent versions of
  the *process* package.
