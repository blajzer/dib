module Main where

import Dib
import Dib.Actions
import Dib.Funcs
import Dib.Rules
import Dib.Targets

import Data.List
import Control.Monad
import System.Environment as Env
import System.Cmd (system)

cleanTarget = makeDummyTarget "clean" "rm -rf testSrc/*.o testSrc/luagame"

compileTarget = makeGxxBuildTarget "testSrc/luagame" "-Wall -O3" "-llua -lGL -lGLU -lSDL -lSDL_mixer -lSDL_image -lSDL_ttf"

main = do
    args <- Env.getArgs
    execTarget cleanTarget
    srcFiles <- rGetFilesInDir "testSrc"
    execTarget $ compileTarget srcFiles
    return ()

