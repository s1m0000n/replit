module Config where
import System.Console.ANSI

promptSeparatorColor = Green
promptSeparator = "𝛌"
promptCommandColor = Blue
promptPathColor = Blue
promptFmt = "[$path] $cmd $predefinedArgs $sep " -- TODO: use this for prompt

shellPrefix = "!"
commandPrefix = ":"
