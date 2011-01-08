module Dib.Funcs where

import Dib
import Data.List
import System.Directory as D
import System.FilePath
import System.Log.Logger as L

-- functions to handle recursively spidering a directory
filePathDeterminer f = D.doesDirectoryExist f >>= \d -> return (f, d)

filePathFilter f = filter noSpecialOrHiddenDirs f
    where noSpecialOrHiddenDirs (x:xs) = x /= '.'
          noSpecialOrHiddenDirs [] = False

directorySplitter :: [(FilePath, Bool)] -> ([FilePath], [FilePath])
directorySplitter dC = foldl' splitter ([], []) dC
    where splitter (d, f) (path, dir) = if dir then (d ++ [path], f) else (d, f ++ [path]) 

fixFilePaths root = map (root </>)

rGetFilesInDir dir = do
    contents <- D.getDirectoryContents dir
    filtContents <- mapM filePathDeterminer $ fixFilePaths dir $ filePathFilter contents
    let (dirs, files) = directorySplitter filtContents in
        do spideredDirs <- mapM rGetFilesInDir dirs
           return $ concat spideredDirs ++ files
           
-- type for a function
defaultLoggingFunc (cmd, output, component) = do
    L.infoM ("Dib." ++ component) output
    return cmd
