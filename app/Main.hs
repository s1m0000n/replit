module Main where

import System.Environment (getArgs)
import Lib
import System.Console.Isocline (setHistory, setPromptMarker, putFmtLn)
import Config
import Utils

main :: IO ()
main = do
    args <- getArgs
    case length args of
      0 -> putFmtLn $ err "At least a single argument must be passed to use replit - runnable shell command such as a program or path to executable"
      _ -> runRepl (head args) (tail args)
    putFmtLn $ err "Something really bad happened, this could not be reached under normal conditions :("
    putFmtLn $ err "Quitting..."
