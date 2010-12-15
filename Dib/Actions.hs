module Dib.Actions where

import Dib
import System.Cmd (system)


data CompilerActionable = CompilerActionable {
   compilerBin :: FilePath,
   compilerFlags :: String,
   compilerCmdFunc :: CompilerActionable -> SrcTransform -> String
   }

instance Actionable CompilerActionable where
    execAction a files = let compilationStrings = map (compilerCmdFunc a a) files in mapM_ system compilationStrings 


data LinkerActionable = LinkerActionable {
    linkerBin :: FilePath,
    linkerFlags :: String,
    linkerCmdFunc :: LinkerActionable -> SrcTransform -> String
    }

instance Actionable LinkerActionable where
    execAction a files = undefined
