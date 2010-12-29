module Dib where

import Data.Array
import Data.List
import Data.Maybe
import System.Cmd (system)
import System.Directory as D
import System.FilePath
import Text.Regex.Base
import Text.Regex.PCRE
import Text.Regex.PCRE.String

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
type GatherFunc = [FilePath] -> [FilePath]
type RuleFunc = String -> (Array Int (MatchOffset, MatchLength)) -> Maybe String
type BindFunc = [String] -> [SrcTransform]

data Rule = GenericRule String RuleFunc BindFunc
          | ReplaceExtensionRule String String
                                            
-- Actions have an internal Rule, if it matches, the Action executes (through this interface)
class Actionable a where
    execAction :: a -> [SrcTransform] -> IO ()

-- Actions have a Rule that gathers files and an Actionable
data Actionable a => Action a = Action Rule a

f --> g = g.f

-- rule evaluation function
evalRule :: String -> Rule -> Maybe String
evalRule s (GenericRule r f b) = f s (s =~ r)
evalRule s (ReplaceExtensionRule r e) = let matchVal = s =~ r :: String
                                        in if matchVal /= "" then
                                            Just (replaceExtension s e)
                                        else
                                            Nothing

--function to gather all of the files an action will operate on
gatherFilesByRule :: Rule -> [FilePath] -> [SrcTransform]
gatherFilesByRule r files = foldl' f [] files
    where f xs path = if (isJust ruleResult) then xs ++ [(OneToOne path (fromJust ruleResult))] else xs where ruleResult = evalRule path r

runAction :: Actionable a => Action a -> [FilePath] -> IO [FilePath]
runAction (Action r impl) files = let gatheredFiles = gatherFilesByRule r files in (execAction impl gatheredFiles) >> (return $ extractTargets gatheredFiles)

-- functions to handle recursively spidering a directory
filePathDeterminer f = D.doesDirectoryExist f >>= \d -> return $ (f, d)

filePathFilter f = filter noSpecialOrHiddenDirs f
    where noSpecialOrHiddenDirs (x:xs) = x /= '.'
          noSpecialOrHiddenDirs [] = False

directorySplitter :: [(FilePath, Bool)] -> ([FilePath], [FilePath])
directorySplitter dC = foldl' splitter ([], []) dC
    where splitter (d, f) (path, dir) = if dir then (d ++ [path], f) else (d, f ++ [path]) 

fixFilePaths root paths = map (root </>) paths

rGetFilesInDir dir = do
    contents <- D.getDirectoryContents dir
    filtContents <- mapM filePathDeterminer $ fixFilePaths dir $ filePathFilter contents
    let (dirs, files) = directorySplitter filtContents in
        do spideredDirs <- mapM rGetFilesInDir dirs
           return $ (concat spideredDirs) ++ files
