module State where

type Command = String
type Args = [String]
type Vars = [(String, String)]


data State = State { command :: Command
                   , leftArgs :: Args
                   , definitions :: Vars }
                   -- To be extended with stuff like exit code of previous command, etc.


