-- Copyright (c) 2010-2016 Brett Lajzer
-- See LICENSE for license information.

-- | A trivial builder that copies a directory tree from one location to another.
module Dib.Builders.Copy (
  makeCopyTarget
  ) where

import Dib.Gatherers
import Dib.Types

import qualified Data.Text as T
import qualified System.Directory as D
import System.FilePath as P

copyFunc :: SrcTransform -> IO StageResult
copyFunc (OneToOne s t) = do
  let unpackedTarget = T.unpack t
  let unpackedSource = T.unpack s
  D.createDirectoryIfMissing True $ takeDirectory unpackedTarget
  putStrLn $ "Copying: " ++ unpackedSource ++ " -> " ++ unpackedTarget
  D.copyFile unpackedSource unpackedTarget
  return $ Right (OneToOne t "")
copyFunc _ = return $ Left "Unexpected SrcTransform"

remapFile :: String -> String -> SrcTransform -> SrcTransform
remapFile src dest (OneToOne s _) = OneToOne s $ T.pack $ dest </> makeRelative src (T.unpack s)
remapFile _ _ _ = error "Unhandled SrcTransform"

-- | The 'makeCopyTarget' function makes a target that copies a directory tree.
-- It takes a name, a source directory, destination directory, and gather filter.
makeCopyTarget :: T.Text -> T.Text -> T.Text -> FilterFunc -> [T.Text] -> Target
makeCopyTarget name src dest f extraDeps =
  let stage = Stage "copy" (map $ remapFile (T.unpack src) (T.unpack dest)) return extraDeps copyFunc
  in Target name (const 0) [] [stage] [makeFileTreeGatherer src f]
