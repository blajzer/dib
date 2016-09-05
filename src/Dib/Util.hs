-- Copyright (c) 2010-2016 Brett Lajzer
-- See LICENSE for license information.

-- | Module containing utility functions and common functionality.
module Dib.Util (
  getSubDirectories
  ) where

import Control.Monad
import qualified System.Directory as D

-- | Given a directory, returns the list of subdirectories.
getSubDirectories :: FilePath -> IO [FilePath]
getSubDirectories root = do
  contents <- D.getDirectoryContents root
  let filteredDirs = filter (\x -> x /= "." && x /= "..") contents
  dirs <- filterM (\(x, _) -> D.doesDirectoryExist x) $ zip (map (root ++) filteredDirs) filteredDirs
  return $ map snd dirs
