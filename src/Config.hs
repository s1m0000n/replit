module Config where

import Data.Text
import State
import Utils
import System.Posix (getEnv)

-- Find out more info about configuring with build-in help command: (when implemented)
--     ... ❯ :h config
-- TL;DR:
--     Formatting (color etc.) is set using BBCode (https://github.com/daanx/isocline#bbcode-format)
--     and you can use function `bbcodePad` to pad Strings with formatting:
--     bbcodePad "ansi-mangeta" "Text that will appear in magenta terminal color."
--
--     Please, escape [ with \\ (it's used for formatting by the used isocline lib)
--     "\\[Text in brackets]" is outputted "[Text in brackets]"
--
--     Prompt is rebuilt each time and to modify it you can leverage current program state :: State
--     which contains all env vars (environment), predefined command (command) with args (leftArgs)
--     and user defininitions (definitions). Do not include marker, trailing space (done automatically).
--
--     To apply config you need to rebuild the program, more info on how & why is in the README.md (https://github.com/s1m0000n/replit/blob/master/README.md)

-- General {
-- :: Settings
enableHistory :: Bool
-- Enable history of what you typed and executed
-- Performance impact: probably minimal, check isocline docs perhaps
enableHistory = True

historySize :: Int
-- Prompt history size
-- Performance impact: high numbers will cause high RAM consumption
historySize = 200

persistHistory :: Bool
-- Save history to file to load on the next run
-- Performance impact: probably minimal, check isocline docs perhaps
persistHistory = True

historyPath :: State -> String
-- File to save history, will be created if does not exist.
-- State can be used, for example, to save different histories for each command like in default
historyPath state = "~/.cache/replit/history_" ++ command state ++ ".txt"

tryFindCompletions :: Bool
-- Future feature - try calling man <command>; <command> --help and find completions
-- Performance impact: medium on startup | command change
tryFindCompletions = True

shellPrefix :: String
-- Prefix used for running direct shell commands (without predefined command & args)
-- Examples: 
--     ... ❯ !ls
--     ... ❯ ! git status
shellPrefix = "!"

builtInCommandPrefix :: String
-- Prefix used for running built-in commands
-- Examples:
--     ... ❯ :q
--     ... ❯ : h
-- Performance impact: minimal
builtInCommandPrefix = ":"

-- :: Colors
-- Performance impact: idk, check isocline docs perhaps
highlightColor :: String
-- Used to highight special stuff
highlightColor = "ansi-magenta"

errorColor :: String
-- Errors output color 
errorColor = "ansi-red"

warningColor :: String
-- Warnings output color
warningColor = "ansi-yellow"

infoColor :: String
-- Info messages output color
infoColor = "ansi-blue"
-- } General

-- Prompt {
-- :: Settings
promptMarker :: String
promptMarker = "❯ "

promptMultilineMarker :: String
promptMultilineMarker = "| "

-- Return a formatted string here to be shown before promptMarker. You can use state & IO.
-- Performance impact: depends on what you do here; for default, comparing to just string: minimal
prompt state = do
  pwd <- getEnv "PWD"
  let pwd_ = case pwd of
           Nothing -> ""
           Just p -> bbcodePad promptPwdColor $ "\\[" ++ p ++ "]"
  let cmd = bbcodePad promptCmdColor $ command state
  let args = bbcodePad promptArgsColor $ showListSpaces $ leftArgs state
  return $ pwd_++ " " ++ cmd ++ " " ++ args

-- :: Colors
-- Performance impact: idk, check isocline docs perhaps
promptPwdColor :: String
promptPwdColor = infoColor

promptCmdColor :: String
promptCmdColor = highlightColor

promptArgsColor :: String
promptArgsColor = promptCmdColor
-- } Prompt
