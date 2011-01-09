module Dib.Actions (
    CompilerActionable(CompilerActionable),
    LinkerActionable(LinkerActionable) 
    ) where

import Dib
import System.Cmd (system)

--TODO: is it possible to do something about the duplication here?

data CompilerActionable = CompilerActionable {
   compilerBin :: FilePath,
   compilerFlags :: String,
   compilerCmdFunc :: CompilerActionable -> SrcTransform -> String
   }

instance Actionable CompilerActionable where
    execAction a files = let compilationStrings = map (compilerCmdFunc a a) files in mapM_ (\x -> putStrLn x >> system x) compilationStrings 


data LinkerActionable = LinkerActionable {
    linkerBin :: FilePath,
    linkerFlags :: String,
    linkerCmdFunc :: LinkerActionable -> SrcTransform -> String
    }

instance Actionable LinkerActionable where
    execAction a files = let linkerStrings = map (linkerCmdFunc a a) files in mapM_ (\x -> putStrLn x >> system x) linkerStrings


