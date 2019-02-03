module Z3 (checkWithZ3) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
    ( newEmptyMVar
    , putMVar
    , takeMVar
    )
import Control.Exception (evaluate)
import System.IO
    ( BufferMode(..)
    , hClose
    , hGetContents
    , hSetBuffering
    , hPutStrLn
    )
import System.Process
    ( CreateProcess(..)
    , StdStream(..)
    , getProcessExitCode
    , waitForProcess
    , withCreateProcess
    , proc
    )

checkWithZ3 :: String -> IO String
checkWithZ3 script = withCreateProcess (proc "z3" ["-in"])
    { std_in = CreatePipe
    , std_out = CreatePipe
    } $ \(Just hIn) (Just hOut) _ ph -> do
    contents <- hGetContents hOut
    outMVar <- newEmptyMVar
    forkIO $ evaluate (length contents) >> putMVar outMVar ()

    hSetBuffering hIn NoBuffering
    hSetBuffering hOut NoBuffering
    hPutStrLn hIn "(push)"
    hPutStrLn hIn script
    hPutStrLn hIn "(pop)"
    hPutStrLn hIn "(exit)"

    hClose hIn
    takeMVar outMVar
    hClose hOut

    return contents
