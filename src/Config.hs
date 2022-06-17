{-# LANGUAGE OverloadedStrings #-}

module Config where
import System.Console.ANSI
import Data.Text

-- General settings
historySize :: Int
historySize = 10000

tryFindCompletions :: Bool
tryFindCompletions = True

-- General colors
highlightColor :: Text
highlightColor = "ansi-magenta"
errorColor :: Text
errorColor = "ansi-red"
warningColor :: Text
warningColor = "ansi-yellow"
infoColor :: Text
infoColor = "ansi-blue"

-- Prompt settings
promptPwdColor :: Text
promptPwdColor = infoColor
promptCmdColor :: Text
promptCmdColor = highlightColor
promptArgsColor :: Text
promptArgsColor = promptCmdColor
promptMarker :: Text
promptMarker = "𝛌 "
promptMultilineMarker :: Text
promptMultilineMarker = "| "
promptFmt :: Text
promptFmt = "\\[$pwd] $cmd $args"

-- Special commands settings
shellPrefix = "!"
commandPrefix = ":"

