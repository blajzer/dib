module Dib.Gatherers(
  SingleFileGatherer(SingleFileGatherer),
  DirectoryGatherer(DirectoryGatherer),
  FileTreeGatherer(FileTreeGatherer),
  Gatherer(),
  wrapGatherStrategy,
  (<++>),
  runGatherers
  )where

import Control.Monad
import Dib.Types
import Data.List
import qualified Data.Text as T
import qualified System.Directory as D
import System.FilePath

instance GatherStrategy SingleFileGatherer where
  gather = singleFileGatherFunc
  
instance GatherStrategy DirectoryGatherer where
  gather = directoryGathererFunc

instance GatherStrategy FileTreeGatherer where
  gather = fileTreeGatherFunc

runGatherers :: [Gatherer] -> IO [T.Text]
runGatherers gs = mapM (\(Gatherer s) -> gather s) gs >>= \x -> foldM (\a b -> return (a ++ b)) [] x

(<++>) :: GatherStrategy s => [Gatherer] -> s -> [Gatherer]
l <++> s = l ++ [wrapGatherStrategy s]

wrapGatherStrategy :: GatherStrategy s => s -> Gatherer
wrapGatherStrategy = Gatherer

singleFileGatherFunc :: SingleFileGatherer -> IO [T.Text]
singleFileGatherFunc (SingleFileGatherer f) = do
  exists <- D.doesFileExist $ T.unpack f
  if exists then return [f] else return []

directoryGathererFunc :: DirectoryGatherer -> IO [T.Text]
directoryGathererFunc (DirectoryGatherer d f) = do
  let unpackedDir = T.unpack d
  contents <- D.getDirectoryContents unpackedDir
  filtContents <- mapM filePathDeterminer $ fixFilePaths unpackedDir $ filePathFilter contents
  let (_, files) = directorySplitter filtContents
  return $ filter f $ map T.pack files

fileTreeGatherFunc :: FileTreeGatherer -> IO [T.Text]
fileTreeGatherFunc (FileTreeGatherer d f) = do
  files <- rGetFilesInDir $ T.unpack d
  return $ filter f $ map T.pack files

-- functions to handle recursively spidering a directory
filePathDeterminer :: FilePath -> IO (FilePath, Bool)
filePathDeterminer f = D.doesDirectoryExist f >>= \d -> return (f, d)

filePathFilter :: [FilePath] -> [FilePath]
filePathFilter f = filter noSpecialOrHiddenDirs f
    where noSpecialOrHiddenDirs (x:_) = x /= '.'
          noSpecialOrHiddenDirs [] = False

directorySplitter :: [(FilePath, Bool)] -> ([FilePath], [FilePath])
directorySplitter dC = foldl' splitter ([], []) dC
    where splitter (d, f) (path, dir) = if dir then (d ++ [path], f) else (d, f ++ [path]) 

fixFilePaths :: FilePath -> [FilePath] -> [FilePath]
fixFilePaths root = map (root </>)

-- | Recursively gets all files in the given directory and its subdirectories.
rGetFilesInDir :: FilePath -> IO [FilePath]
rGetFilesInDir dir = do
    contents <- D.getDirectoryContents dir
    filtContents <- mapM filePathDeterminer $ fixFilePaths dir $ filePathFilter contents
    let (dirs, files) = directorySplitter filtContents
    spideredDirs <- mapM rGetFilesInDir dirs
    return $ concat spideredDirs ++ files