module Lib where

import System.Console.ANSI
import GHC.IO.Handle.FD (stdout)
import GHC.IO.Handle (hFlush)
import System.Process
import System.Directory
import System.Exit (exitSuccess)
import Config
import Data.List (maximumBy)
import Data.Function (on)

replitMsg :: String -> String -> String
replitMsg "" text = "[replit] " ++ text
replitMsg prefix text = '[':prefix ++ "@replit] " ++ text

putErrLn :: String -> IO ()
putErrLn text = do
  setSGR [SetColor Foreground Dull Red]
  putStrLn $ replitMsg "error" text
  setSGR [Reset]

putWarnLn :: String -> IO ()
putWarnLn text = do
  setSGR [SetColor Foreground Dull Yellow]
  putStrLn $ replitMsg "warning" text
  setSGR [Reset]

putInfo :: String -> IO ()
putInfo text = do
  setSGR [SetColor Foreground Dull Blue]
  putStr text
  setSGR [Reset]

putInfoLn :: String -> IO ()
putInfoLn text = do
  setSGR [SetColor Foreground Dull Blue]
  putStrLn $ replitMsg "info" text
  setSGR [Reset]

putSpecial :: String -> IO ()
putSpecial text = do
  setSGR [SetColor Foreground Dull Magenta]
  putStr text
  setSGR [Reset]

cput :: Color -> String -> IO ()
cput color text = do
  setSGR [SetColor Foreground Dull color]
  putStr text
  setSGR [Reset]

cputLn :: Color -> String -> IO ()
cputLn color text = do
  setSGR [SetColor Foreground Dull color]
  putStrLn text
  setSGR [Reset]

handleCtrlC :: String -> [String] -> IO ()
handleCtrlC executable options = do
  putStrLn ""
  putWarnLn "To quit please use '!! q' (see more in help)"
  repl executable options

mult lst times = concat $ replicate times lst

showArgs :: [String] -> String
showArgs args = if length args > 0 then (foldl1 (\acc x -> acc ++ ", " ++ x) args) else ""

showHowCalledArgs :: [String] -> String
showHowCalledArgs args = if length args > 0 then (foldl1 (\acc x -> acc ++ " " ++ x) args) else ""

pad :: [a] -> [a] -> [a]
pad _ [] = []
pad padElems mainElems = padElems ++ mainElems ++ padElems

startInfo :: String -> [String] -> IO ()
startInfo executable args = do
  putInfo "[info@replit] replit will run"
  putSpecial $ ' ':executable
  if length args > 0
     then putSpecial $ pad " " $ showHowCalledArgs args
     else putStr " "
  putInfo $ "[user specified args after '" ++ promptSeparator ++ "']"
  putStrLn ""
  putInfoLn "The basic commands are !h to print help, !q to quit, !sh [command] to run any shell command (w/o prefix)"

help :: IO()
help = putErrLn "Implement help"

-- data CText = Color String

-- commandHelp :: String -> CText
-- commandHelp "q" = CText White "Quit replit"
-- commandHelp cmd = CText Red $ "Help for '" ++ cmd ++ "' command is not implemeted"

exec :: FilePath -> [String] -> IO ()
exec executable args = do
  handle <- spawnProcess executable args
  waitForProcess handle
  return ()

execSpecial :: String -> p1 -> p2 -> [String] -> IO ()
execSpecial "q" _ _ [] = do
  putStrLn "Quitting..."
  exitSuccess
execSpecial "h" executable predefinedArgs [] = help
-- execSpecial "h" executable predefinedArgs [cmd] = commandHelp cmd
execSpecial "l" executable predefinedArgs [name, "=", value] = putErrLn $ "Implement define " ++ name ++ " = '" ++ value ++ "'"
execSpecial "btw_i_use_arch" _ _ _ =  do
  setSGR [SetColor Foreground Dull Black]
  setSGR [SetColor Background Vivid Red]
  putStrLn $ "R" `mult` 20
  setSGR [SetColor Background Vivid Yellow]
  putStrLn $ "A" `mult` 20
  setSGR [Reset]
execSpecial command executable predefinedArgs args = do
  putErrLn $  "Unknown special command " ++ command ++ " with args " ++ (showArgs args) ++ ". Please use one of the supported commands"
  help

putPrompt :: String -> [String] -> IO ()
putPrompt executable predefinedArgs = do
  let argstr = showHowCalledArgs predefinedArgs
  let cmd = executable ++ if argstr == "" then "" else " " ++ argstr
  pwd <- getCurrentDirectory
  cput promptPathColor $ "[" ++ pwd ++ "] "
  if cmd == ""
     then cput promptSeparatorColor $ promptSeparator ++ " "
     else do
       cput promptCommandColor cmd
       cput promptSeparatorColor $ pad " " promptSeparator

trimPrefix :: Eq a => [a] -> [a] -> (Bool, [a])
trimPrefix (p:ps) (x:xs) 
  | p == x = trimPrefix ps xs
  | otherwise = (False, xs)
trimPrefix [] xs = (True, xs)

specialPrefixes :: [String]
specialPrefixes = [shellPrefix, commandPrefix]

findPrefix :: [Char] -> Maybe (String, String)
findPrefix xs = 
  if length matchedPrefixesMap == 0
     then Nothing
     else if (body == "") then Nothing else Just (prefix, body)
    where
      matchedPrefixesMap = map (\(_, k, v) -> (k, v)) 
        $ filter (\(b, k, v) -> b == True) 
        $ zipWith (\k (b, v) -> (b, k, v)) specialPrefixes 
        $ map (\ps -> trimPrefix ps xs) specialPrefixes
      (prefix, body) = maximumBy (compare `on` (\(k, v) -> length v)) matchedPrefixesMap 

tokenize :: String -> [String]
tokenize text = case (findPrefix firstToken) of
    Nothing -> tokens
    Just (tokenPrefix, tokenTail) -> tokenPrefix:tokenTail:otherTokens
 where 
  tokens = words text
  firstToken = head tokens
  otherTokens = tail tokens
  
repl :: String -> [String] -> IO()
repl executable predefinedArgs = do
  putPrompt executable predefinedArgs
  hFlush stdout
  line <- getLine
  if line == ""
     then do
       exec executable predefinedArgs
     else do
       let tokens = tokenize line
       let firstToken = head tokens
       case firstToken of
         _ | firstToken == shellPrefix -> exec (head $ tail tokens) (tail $ tail tokens)
         _ | firstToken == commandPrefix -> execSpecial (head $ tail tokens) executable predefinedArgs (tail $ tail tokens)
         _ -> exec executable $ predefinedArgs ++ tokens 
  repl executable predefinedArgs

