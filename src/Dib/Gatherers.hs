{-# OPTIONS_GHC -fno-warn-orphans #-}
-- Copyright (c) 2010-2016 Brett Lajzer
-- See LICENSE for license information.

-- | Module that exposes all of the various 'GatherStrategy' types and functions
-- for dealing with them.
module Dib.Gatherers(
  SingleFileGatherer(),
  DirectoryGatherer(),
  FileTreeGatherer(),
  Gatherer(),
  CommandGatherer(),
  wrapGatherStrategy,
  runGatherers,
  makeSingleFileGatherer,
  makeDirectoryGatherer,
  makeFileTreeGatherer,
  makeCommandGatherer,
  matchAll,
  matchExtension,
  matchExtensionExcluded,
  matchExtensions,
  matchExtensionsExcluded
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

instance GatherStrategy CommandGatherer where
  gather = commandGatherFunc

-- | Runs a list of 'Gatherer's and returns the concatenation of their output.
runGatherers :: [Gatherer] -> IO [T.Text]
runGatherers gs = mapM (\(Gatherer s) -> gather s) gs >>= \x -> foldM (\a b -> return (a ++ b)) [] x

-- | Convenience function to turn a 'GatherStrategy' into a 'Gatherer'.
wrapGatherStrategy :: GatherStrategy s => s -> Gatherer
wrapGatherStrategy = Gatherer

-- | Constructs a 'Gatherer' that returns a single file.
makeSingleFileGatherer :: T.Text -> Gatherer
makeSingleFileGatherer = wrapGatherStrategy.SingleFileGatherer

-- | Constructs a 'Gatherer' that returns all files in a directory (but not its
-- subdirectories) that match a given filter.
makeDirectoryGatherer :: T.Text -> FilterFunc -> Gatherer
makeDirectoryGatherer d = wrapGatherStrategy.DirectoryGatherer d

-- | Constructs a 'Gatherer' that returns all files in a directory tree that
-- match a given filter.
makeFileTreeGatherer :: T.Text -> FilterFunc -> Gatherer
makeFileTreeGatherer d = wrapGatherStrategy.FileTreeGatherer d

-- | Constructs a 'Gatherer' that runs an arbitrary 'IO' action.
makeCommandGatherer :: IO () -> Gatherer
makeCommandGatherer = wrapGatherStrategy.CommandGatherer

singleFileGatherFunc :: SingleFileGatherer -> IO [T.Text]
singleFileGatherFunc (SingleFileGatherer f) = do
  exists <- D.doesFileExist $ T.unpack f
  return [f | exists]

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

commandGatherFunc :: CommandGatherer -> IO [T.Text]
commandGatherFunc (CommandGatherer f) = do
  f
  return []

-- functions to handle recursively spidering a directory
filePathDeterminer :: FilePath -> IO (FilePath, Bool)
filePathDeterminer f = D.doesDirectoryExist f >>= \d -> return (f, d)

filePathFilter :: [FilePath] -> [FilePath]
filePathFilter = filter noSpecialOrHiddenDirs
    where noSpecialOrHiddenDirs (x:_) = x /= '.'
          noSpecialOrHiddenDirs [] = False

directorySplitter :: [(FilePath, Bool)] -> ([FilePath], [FilePath])
directorySplitter = foldl' splitter ([], [])
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

-- | Filter function that returns all files.
matchAll :: FilterFunc
matchAll _ = True

-- | Filter function that returns files with a given extension.
matchExtension :: T.Text -> FilterFunc
matchExtension = T.isSuffixOf

-- | Filter function that returns files that match any of a list of extensions.
matchExtensions :: [T.Text] -> FilterFunc
matchExtensions exts file = foldl' foldFunc False exts
  where foldFunc True _ = True
        foldFunc False e = e `T.isSuffixOf` file

-- | Filter function that returns files with a given extension that don't match exclusion rules.
matchExtensionExcluded :: T.Text -> [T.Text -> Bool] -> FilterFunc
matchExtensionExcluded ext rules file = (T.isSuffixOf ext file) && (not.or $ map (\r -> r file) rules)

-- | Filter function that returns files that match any of a list of extensions, but don't match the exclusion rules.
matchExtensionsExcluded :: [T.Text] -> [T.Text -> Bool] -> FilterFunc
matchExtensionsExcluded exts rules file = foldl' foldFunc False exts
  where foldFunc True _ = True
        foldFunc False e = (e `T.isSuffixOf` file) && (not.or $ map (\r -> r file) rules)
