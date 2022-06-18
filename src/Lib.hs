module Lib where

import GHC.IO.Handle.FD (stdout, stdin)
import GHC.IO.Handle (hFlush)
import System.Process (createProcess, proc, shell, waitForProcess)
import System.Directory (doesFileExist, makeRelativeToCurrentDirectory)
import System.Exit (exitSuccess)
import Config
import System.Console.Isocline (readline, historyAdd, putFmt, putFmtLn, setHistory, setPromptMarker)
import Utils 
import State
import Control.Exception (handle)
import InputProcessing (tokenize)
import System.Posix (installHandler, keyboardSignal, Handler (Catch), getEnvironment, getEnv)

err = bbcodePad errorColor . ("\\[replit error] " ++)
warn = bbcodePad warningColor . ("\\[replit warning] " ++)
info = bbcodePad infoColor . ("\\[replit info] " ++)
highlighted = bbcodePad highlightColor

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

exec :: State -> Args -> IO State
exec state rightArgs = do
  let args = showListSpaces $ leftArgs state ++ rightArgs
  let shellLine = command state ++ " " ++ args
  maybeShell <- getEnv "SHELL"
  (_, _, _, handle) <- case maybeShell of
                         Nothing -> createProcess $ shell shellLine
                         Just sh -> createProcess $ proc sh ["-c", shellLine]
  waitForProcess handle
  return state

execBuiltIn :: State -> Command -> Args -> IO State
execBuiltIn _ "q" _ = do
  putFmtLn $ info "Quitting..."
  exitSuccess

execBuiltIn state "h" [] = do
  putFmtLn $ info "replit - A tiny tool to make your favorite CLI-apps interactive"
  putFmtLn $ err "Help is not implemented yet :("
  return state

execBuiltIn state command args = do
  putFmtLn 
     $ (err "Unknown built-in command ")
    ++ (highlighted command)
    ++ (err " with args ")
    ++ (highlighted $ showListSpaces args) 
    ++ (err ". Please help to see a list of valid commands.")
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
                 exec state{command = "mkdir", leftArgs = []} ["-p", "$(dirname " ++ path ++ ")"]
                 exec state{command = "touch", leftArgs = []} [path]
               else return state
            return path
           else return "1"
       setHistory path historySize
       return state
     else return state

repl :: State -> IO ()
repl state = do
  -- env <- if useEnvironment && updateEnvironment then getEnvironment else return []
  line <- runCommandLine state
  state <- if line == ""
     then exec state []
     else let
       tokens = tokenize line
       firstToken = head tokens
     in case firstToken of
       _ | firstToken == shellPrefix -> exec state{command = head $ tail tokens, leftArgs = tail $ tail tokens} []
       _ | firstToken == builtInCommandPrefix -> execBuiltIn state (head $ tail tokens) (tail $ tail tokens)
       _ -> exec state tokens 
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

