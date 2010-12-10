module Main where

import Control.Monad
import Data.Array
import Data.Maybe
import Data.List
import System.Directory as D
import System.Environment as Env
import System.FilePath
import Text.Regex.Base
import Text.Regex.PCRE
import Text.Regex.PCRE.String

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

-- A function used by a Rule to piece together the result String
type RuleFunc = String -> (Array Int (MatchOffset, MatchLength)) -> Maybe String
data Rule = GenericRule String RuleFunc
          | ReplaceExtensionRule String String

-- rule evaluation function
evalRule :: String -> Rule -> Maybe String
evalRule s (GenericRule r f) = f s (s =~ r)
evalRule s (ReplaceExtensionRule r e) = let matchVal = s =~ r :: String
                                        in if matchVal /= "" then
                                            Just (replaceExtension s e)
                                        else
                                            Nothing

-- Actions have an internal Rule, if it matches, the Action executes
class Actionable a where
    execAction :: a -> [(FilePath, FilePath)] -> IO ()

data Actionable a => Action a = Action Rule a

--function to gather all of the files an action will operate on
gatherFilesByRule :: Rule -> [FilePath] -> [(FilePath, FilePath)]
gatherFilesByRule r files = foldl' f [] files
    where f xs path = if (isJust ruleResult) then xs ++ [(path, (fromJust ruleResult))] else xs where ruleResult = evalRule path r

main = do
    args <- Env.getArgs
    mapM_ putStrLn args
    return ()
