{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import Lib
import System.Posix (installHandler, keyboardSignal, Handler (Catch))
import System.Console.Isocline (setHistory, setPromptMarker, putFmtLn)
import Config
import qualified Data.Text as T

main :: IO ()
main = do
    args <- getArgs
    case length args of
      0 -> do
        putFmtLn $ T.unpack $ bbpad errorColor $ promptMarker `T.append` "At least a single argument must be passed to use replit - runnable shell command such as a program or path to executable"
        help
      otherwise -> do
        let executable = head args
        let options = tail args
        installHandler keyboardSignal (Catch $ handleCtrlC executable options) Nothing
        startInfo executable options
        setHistory "~/.cache/replit/history.txt" historySize
        setPromptMarker (T.unpack promptMarker) (T.unpack promptMultilineMarker)
        -- add completions from --help if available 
        repl executable options
    putInfoLn "Quitting..."
