module Utils where

showListSpaces :: [String] -> String
showListSpaces [] = ""
showListSpaces (x:xs) = x ++ " " ++ (showListSpaces xs)

bbcodePad :: String -> String -> String
bbcodePad "" text = text
bbcodePad properties text = "[" ++ properties ++ "]" ++ text ++ "[/" ++ properties ++ "]"


