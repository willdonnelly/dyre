{-# LANGUAGE CPP #-}

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
{-# LANGUAGE ForeignFunctionInterface #-}
#endif


{- |
Compatibility code for things that need to be done differently
on different systems.
-}
module Config.Dyre.Compat ( customExec, getPIDString ) where

import Config.Dyre.Options ( customOptions )

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)

-- Windows

import System.Win32
import System.Process
import System.Exit
import System.Mem

-- This can be removed as soon as a 'getProcessID' function
-- gets added to 'System.Win32'
foreign import stdcall unsafe "winbase.h GetCurrentProcessId"
    c_GetCurrentProcessID :: IO DWORD
getPIDString = fmap show c_GetCurrentProcessID

customExec binary mArgs = do
    args <- customOptions mArgs
    -- This whole thing is a terrible, ugly hack. Since Windows
    -- is too braindead to provide an exec() system call for us
    -- to use, we simply create a new process that inherits
    -- the stdio handles.
    (_,_,_,child) <- createProcess $ CreateProcess
        { cmdspec   = RawCommand binary args
        , cwd       = Nothing
        , env       = Nothing
        , std_in    = Inherit
        , std_out   = Inherit
        , std_err   = Inherit
        , close_fds = True
        , create_group = False
        }
    -- Do some garbage collection in an optimistic attempt to
    -- offset some of the memory we waste here.
    performGC
    -- And to prevent terminal apps from losing IO, we have to
    -- sit around waiting for the child to exit.
    exitCode <- waitForProcess child
    case exitCode of
         ExitSuccess -> c_ExitProcess 0
         ExitFailure c -> c_ExitProcess (fromIntegral c)

foreign import stdcall unsafe "winbase.h ExitProcess"
    c_ExitProcess :: UINT -> IO ()

#else

import System.Posix.Process ( executeFile, getProcessID )

#ifdef darwin_HOST_OS

import System.Posix.Process
  ( exitImmediately , forkProcess, getProcessStatus, ProcessStatus(..) )
import System.Posix.Signals ( raiseSignal, sigTSTP )
import System.Exit          ( ExitCode(ExitSuccess) )

-- OSX.  In a threaded process execv fails with ENOTSUP.
-- See http://uninformed.org/index.cgi?v=1&a=1&p=16.  So it
-- is necessary to fork _then_ exec.
--
-- According to https://bugs.python.org/issue6800 this was
-- fixed in OS X 10.6.  But I guess we'll leave the workaround
-- in place until there is a compelling reason to remove it.

customExec binary mArgs = do
    args <- customOptions mArgs
    childPID <- forkProcess $ executeFile binary False args Nothing
    forever $ do
        childStatus <- getProcessStatus True True childPID
        case childStatus of
             Nothing -> error "executeFile: couldn't get child process status"
             Just (Exited code) -> exitImmediately code
#if MIN_VERSION_unix(2,7,0)
             Just (Terminated _ _) -> exitImmediately ExitSuccess
#else
             Just (Terminated _) -> exitImmediately ExitSuccess
#endif
             Just (Stopped _) -> raiseSignal sigTSTP
  where forever a = a >> forever a

#else

-- Linux / BSD

customExec binary mArgs = do
   args <- customOptions mArgs
   executeFile binary False args Nothing

#endif

getPIDString = fmap show getProcessID

#endif


-- | Called whenever execution needs to be transferred over to
--   a different binary.
customExec :: FilePath -> Maybe [String] -> IO ()

-- | What it says on the tin. Gets the current PID as a string.
--   Used to determine the name for the state file during restarts.
getPIDString :: IO String
