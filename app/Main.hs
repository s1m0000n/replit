module Main where

import System.Environment (getArgs)
import Lib
import System.Posix (installHandler, keyboardSignal, Handler (Catch))

main :: IO ()
main = do
    args <- getArgs
    case length args of
      0 -> do
        putErrLn "At least a single argument must be passed to use replit - runnable shell command such as a program or path to executable"
        help
      otherwise -> do
        let executable = head args
        let options = tail args
        installHandler keyboardSignal (Catch $ handleCtrlC executable options) Nothing
        -- TODO: https://stackoverflow.com/questions/13441676/how-to-write-ctrl-c-handler-in-haskell
        startInfo executable options
        repl executable options
    putInfoLn "Quitting..."
