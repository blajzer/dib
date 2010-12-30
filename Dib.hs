module Dib where

import Data.Array
import Data.List
import Data.Maybe
import System.Cmd (system)
import System.FilePath

-- Data type for mapping between src and destination files
data SrcTransform = OneToOne FilePath FilePath
                  | OneToMany FilePath [FilePath]
                  | ManyToOne [FilePath] FilePath
                  | ManyToMany [FilePath] [FilePath]
                  deriving (Show)

-- extracts a list of targets from a list of SrcTransforms
extractTargets xs = foldl' f [] xs
    where f targets (OneToOne s d) = targets ++ [d]
          f targets (OneToMany s d) = targets ++ d
          f targets (ManyToOne s d) = targets ++ [d]
          f targets (ManyToMany s d) = targets ++ d

-- A function used by a Rule to piece together the result String
type GatherFunc = [FilePath] -> IO [FilePath]
                                            
class Actionable a where
    execAction :: a -> [SrcTransform] -> IO ()

class Rule r where
    evalRule :: r -> [FilePath] -> [SrcTransform]

-- Actions have a Rule that groups files into a set of transformations
--  and an Actionable that processes these transformations
data (Actionable a, Rule r) => Action r a = Action r a

runAction :: (Actionable a, Rule r) => Action r a -> [FilePath] -> IO [FilePath]
runAction (Action rule impl) files = let gatheredFiles = evalRule rule files in (execAction impl gatheredFiles) >> (return $ extractTargets gatheredFiles)

