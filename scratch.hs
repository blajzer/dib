module Main where

import Dib
import Dib.Actions
import Dib.Funcs
import Dib.Rules

import Data.List
import Control.Monad
import System.Environment as Env
import System.Cmd (system)

gxxCmdFunc a (OneToOne src dest) = compilerBin a ++ " " ++ compilerFlags a ++ " -o " ++ dest ++ " -c " ++ src

gxxCompiler = CompilerActionable {
    compilerBin = "g++",
    compilerFlags = "-Wall -O3",
    compilerCmdFunc = gxxCmdFunc
}

cppCompileAction = Action (ReplaceExtensionRule "cpp" "o") gxxCompiler

gxxLinkFunc a (ManyToOne src dest) = linkerBin a ++ " " ++ linkerFlags a ++ " -o " ++ dest ++ " " ++ unwords src 

gxxLinker = LinkerActionable {
    linkerBin = "g++",
    linkerFlags = "-Wall -O3 -llua -lGL -lGLU -lSDL -lSDL_mixer -lSDL_image -lSDL_ttf",
    linkerCmdFunc = gxxLinkFunc
    }

cppLinkAction = Action (ManyToOneRule "testSrc/luagameDib") gxxLinker

main = do
    args <- Env.getArgs
    srcFiles <- rGetFilesInDir "testSrc"
    objFiles <- runAction cppCompileAction srcFiles
    runAction cppLinkAction objFiles
    return ()
