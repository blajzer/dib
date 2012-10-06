module Dib.Util (
  getSubDirectories
  ) where

import Control.Monad
import qualified System.Directory as D

getSubDirectories :: FilePath -> IO [FilePath]
getSubDirectories root = do
  contents <- D.getDirectoryContents root
  let filteredDirs = filter (\x -> x /= "." && x /= "..") contents
  dirs <- filterM (\(x, _) -> D.doesDirectoryExist x) $ zip (map (root ++) filteredDirs) filteredDirs
  return $ map snd dirs
