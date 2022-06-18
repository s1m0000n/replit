module Lib where

import GHC.IO.Handle.FD (stdout, stdin)
import GHC.IO.Handle (hFlush)
import System.Process (createProcess, proc, shell, waitForProcess)
import System.Directory (doesFileExist, makeRelativeToCurrentDirectory)
import System.Exit (exitSuccess, ExitCode)
import Config
import System.Console.Isocline (readline, historyAdd, putFmt, putFmtLn, setHistory, setPromptMarker)
import Utils 
import State
import Control.Exception (handle)
import InputProcessing (tokenize)
import System.Posix (installHandler, keyboardSignal, Handler (Catch), getEnvironment, getEnv, changeWorkingDirectory)
import Data.List (isSubsequenceOf)

err :: String -> String
err = bbcodePad errorColor . ("\\[replit error] " ++)

warn :: String -> String
warn = bbcodePad warningColor . ("\\[replit warning] " ++)

info :: String -> String
info = bbcodePad infoColor . ("\\[replit info] " ++)

highlighted :: String -> String
highlighted = bbcodePad highlightColor

handleCtrlC :: State -> IO ()
handleCtrlC state = do
  putStrLn ""
  putFmtLn $ warn "To quit please use '!! q' (more info in help)"
  repl state

pad :: [a] -> [a] -> [a]
pad _ [] = []
pad padElems mainElems = padElems ++ mainElems ++ padElems

startInfo :: State -> IO ()
startInfo state = do
  putFmt $ info "replit will run"
  putFmt $ bbcodePad highlightColor $ pad " " $ command state
  putFmt $ bbcodePad highlightColor $ showListSpaces $ leftArgs state
  putFmtLn $ bbcodePad infoColor $ "\\[optional user specified args after '" ++ promptMarker ++ "']"
  putFmtLn $ info $ "The basic commands are " ++ builtInCommandPrefix ++ "h to print help, " ++ builtInCommandPrefix ++ "q to quit, !\\[command] to run any shell command (w/o predefined stuff)"

exec :: Command -> Args -> IO ExitCode
exec cmd args = do
  if cmd == "cd"
     then changeWorkingDirectory (head args)
     else return ()
  let shellLine = cmd ++ " " ++ (showListSpaces args)
  maybeShell <- getEnv "SHELL"
  (_, _, _, handle) <- case maybeShell of
                         Nothing -> createProcess $ shell shellLine
                         Just sh -> createProcess $ proc sh ["-c", shellLine]
  waitForProcess handle -- sometime move it and make async execution? 

execBuiltIn :: State -> Command -> Args -> IO State
execBuiltIn state cmd args
  | cmd `isSubsequenceOf` "quit" = do
      putFmtLn $ info "Quitting..."
      exitSuccess
  | cmd `isSubsequenceOf` "help" = do
      case args of
        [] ->  putFmtLn $ err "Help is not implemented yet :("
        _ -> putFmtLn $ err "Command help is not implemented yet"
      return state
  | otherwise = do
      putFmtLn $ (err "Unknown built-in command ") ++ (highlighted cmd) ++ (err " with args ") ++ (highlighted $ showListSpaces args) ++ (err ". Please help to see a list of valid commands.")
      return state

runCommandLine :: State -> IO String
runCommandLine state = do
  promptText <- prompt state
  line <- readline promptText
  historyAdd line
  return line

initHistory :: State -> IO State
initHistory state = if enableHistory
     then do
       path <- if persistHistory 
          then do
            let path = historyPath state
            exists <- doesFileExist path
            if not exists 
               then do
                 exec "mkdir" ["-p", "$(dirname " ++ path ++ ")"]
                 exec "touch" [path]
                 return ()
               else return ()
            return path
           else return "1"
       setHistory path historySize
       return state
     else return state

repl :: State -> IO ()
repl state = do
  line <- runCommandLine state
  state <- if line == ""
     then do
       exec (command state) (leftArgs state)
       return state
     else let
       tokens = tokenize line
       firstToken = head tokens
     in case firstToken of
       _ | firstToken == shellPrefix -> do
         exec (head $ tail tokens) $ tail $ tail tokens
         return state
       _ | firstToken == builtInCommandPrefix -> execBuiltIn state (head $ tail tokens) (tail $ tail tokens)
       _ -> do
         exec (command state) $ (leftArgs state) ++ tokens
         return state
  repl state

runRepl :: Command -> Args -> IO ()
runRepl cmd args = do
  -- initialization of state & isocline, set handler
  let state = State{command = cmd, leftArgs = args, definitions = []}
  setPromptMarker promptMarker promptMultilineMarker
  state <- initHistory state
  -- add completions from --help if available 
  startInfo state
  installHandler keyboardSignal (Catch $ handleCtrlC state) Nothing
  repl state

