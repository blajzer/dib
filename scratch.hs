module Main where

import Dib
import Dib.Actions
import Dib.Funcs
import Dib.Rules
import Control.Monad
import System.Environment as Env
import System.Cmd (system)

gxxCmdFunc a (OneToOne src dest) = (compilerBin a) ++ " " ++ (compilerFlags a) ++ " -o " ++ dest ++ " -c " ++ src

gxxCompiler = CompilerActionable {
    compilerBin = "g++",
    compilerFlags = "-Wall -O3",
    compilerCmdFunc = gxxCmdFunc
}

cppCompileAction = Action (ReplaceExtensionRule "cpp" "o") gxxCompiler

main = do
    args <- Env.getArgs
    srcFiles <- rGetFilesInDir "testSrc"
    runAction cppCompileAction srcFiles
    return ()
