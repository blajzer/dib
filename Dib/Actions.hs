module Dib.Actions (
    CompilerActionable(CompilerActionable, compilerBin, compilerFlags, compilerCmdFunc),
    gxxCompiler,
    CommandActionable(CommandActionable)
    ) where

import Dib
import System.Cmd (system)

data CompilerActionable = CompilerActionable {
   compilerBin :: FilePath,
   compilerFlags :: String,
   compilerCmdFunc :: CompilerActionable -> SrcTransform -> String
   }

instance Actionable CompilerActionable where
	generateActionCmd a = compilerCmdFunc a a

gxxCmdFunc a (OneToOne src dest) = compilerBin a ++ " " ++ compilerFlags a ++ " -o " ++ dest ++ " -c " ++ src
gxxCmdFunc a (ManyToOne src dest) = compilerBin a ++ " " ++ compilerFlags a ++ " -o " ++ dest ++ " " ++ unwords src 

gxxCompiler = CompilerActionable {
    compilerBin = "g++",
    compilerFlags = "-Wall -O3",
    compilerCmdFunc = gxxCmdFunc
    }
 
-- | A CommandActionable is an action that just runs the command string with no changes.
data CommandActionable = CommandActionable String

instance Actionable CommandActionable where
	generateActionCmd (CommandActionable cmd) _ = cmd
