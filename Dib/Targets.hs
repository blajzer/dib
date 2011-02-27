
module Dib.Targets (Target(Target),
                    Stage(Stage),
                    makeDummyTarget,
                    makeGxxBuildTarget,
                    execTarget
                    ) where

import Control.Monad
import Data.Maybe
import Dib
import Dib.Actions
import Dib.Rules

-- | Represents an entire chain of build actions 
--   which go from a set of input files to ouput files.
data Target = Target String [Stage] [FilePath]

-- | Represents one stage in the chain of build actions.
data Stage = Stage ([FilePath] -> IO (Maybe [FilePath]))

-- | Makes a build target that always executes a given command.
makeDummyTarget :: String -> String -> Target
makeDummyTarget name cmd = Target name [(Stage (runAction (Action DummyRule (CommandActionable cmd))))] [""]

-- | Makes a compile / link target using the standard g++ compiler. 
makeGxxBuildTarget executable compileFlags linkFlags =
    let
        compiler = gxxCompiler { compilerFlags = compileFlags }
        linker = gxxCompiler { compilerFlags = (compileFlags ++ " " ++ linkFlags) }
        cppCompileAction = Action (ReplaceExtensionRule "cpp" "o") compiler
        cCompileAction = Action (ReplaceExtensionRule "c" "o") compiler
        asmCompileAction = Action (ReplaceExtensionRule "s" "o") compiler
        cppLinkAction = Action (ManyToOneRule executable) linker
        compileFunc files = do
            cppO <- runAction cppCompileAction files
            cO <- runAction cCompileAction files
            asmO <- runAction asmCompileAction files
            return $ combiner cppO cO asmO
        combiner Nothing _ _ = Nothing
        combiner _ Nothing _ = Nothing
        combiner _ _ Nothing = Nothing
        combiner (Just a) (Just b) (Just c) = Just $ a ++ b ++ c
        compileStage = Stage compileFunc
        linkStage = Stage (runAction cppLinkAction)
    in Target executable [compileStage, linkStage] 

-- | Executes a Target.
execTarget :: Target -> IO (Maybe [FilePath])
execTarget (Target name stages files) = do
    putStrLn $ "Building target: " ++ name
    foldM runStage (Just files) stages
    where
        runStage (Just files) (Stage f) = f files
        runStage Nothing _ = return Nothing

