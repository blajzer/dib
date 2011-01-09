module Main where

import Dib
import Dib.Actions
import Dib.Funcs
import Dib.Rules

import Data.List
import Control.Monad
import System.Environment as Env
import System.Cmd (system)

cppCompileAction = Action (ReplaceExtensionRule "cpp" "o") (gxxCompiler { compilerFlags = "-Wall -O3" })
cppLinkAction = Action (ManyToOneRule "testSrc/luagameDib") (gxxCompiler { compilerFlags = "-Wall -O3 -llua -lGL -lGLU -lSDL -lSDL_mixer -lSDL_image -lSDL_ttf" })

main = do
    args <- Env.getArgs
    srcFiles <- rGetFilesInDir "testSrc"
    objFiles <- runAction cppCompileAction srcFiles
    runAction cppLinkAction objFiles
    --runAction (Action DummyRule (CommandActionable "rm -rf testSrc/*.o testSrc/luagameDib")) [""]
    return ()

