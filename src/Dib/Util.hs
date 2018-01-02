-- Copyright (c) 2010-2018 Brett Lajzer
-- See LICENSE for license information.

-- | Module containing utility functions and common functionality.
module Dib.Util (
  getSubDirectories,
  handleExitCode
  ) where

import Dib.Types

import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import Control.Monad
import qualified Data.Text as T
import qualified System.Directory as D

-- | Given a directory, returns the list of subdirectories.
getSubDirectories :: FilePath -> IO [FilePath]
getSubDirectories root = do
  contents <- D.getDirectoryContents root
  let filteredDirs = filter (\x -> x /= "." && x /= "..") contents
  dirs <- filterM (\(x, _) -> D.doesDirectoryExist x) $ zip (map (root ++) filteredDirs) filteredDirs
  return $ map snd dirs

-- | A utility function for handling 'ExitCode's in 'StageFunction's.
handleExitCode :: ExitCode -> T.Text -> String -> IO StageResult
handleExitCode ExitSuccess t _ = return $ Right $ OneToOne t ""
handleExitCode (ExitFailure _) _ e = return $ Left $ T.pack (show e)
