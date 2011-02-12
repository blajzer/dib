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
cCompileAction = Action (ReplaceExtensionRule "c" "o") (gxxCompiler { compilerFlags = "-Wall -O3" })
asmCompileAction = Action (ReplaceExtensionRule "s" "o") (gxxCompiler { compilerFlags = "-Wall -O3" })
cppLinkAction = Action (ManyToOneRule "testSrc/luagameDib") (gxxCompiler { compilerFlags = "-Wall -O3 -llua -lGL -lGLU -lSDL -lSDL_mixer -lSDL_image -lSDL_ttf" })

data Target = Target String [Stage] [FilePath]
data Stage = Stage ([FilePath] -> IO [FilePath])

cleanStage = Stage (runAction (Action DummyRule (CommandActionable "rm -rf testSrc/*.o testSrc/luagameDib")))
cleanTarget = Target "clean" [cleanStage] [""]

linkStage = Stage (runAction cppLinkAction)

compileStage = Stage compileFunc
compileTarget = Target "luagame" [compileStage, linkStage] 

compileFunc files = do
    cppO <- runAction cppCompileAction files
    cO <- runAction cCompileAction files
    asmO <- runAction asmCompileAction files
    return $ cppO ++ cO ++ asmO

execTarget :: Target -> IO [FilePath]
execTarget (Target name stages files) = do
    putStrLn $ "Building target: " ++ name
    foldM runStage files stages

runStage files (Stage f) = f files

main = do
    args <- Env.getArgs
    execTarget cleanTarget
    srcFiles <- rGetFilesInDir "testSrc"
    execTarget $ compileTarget srcFiles
    return ()

