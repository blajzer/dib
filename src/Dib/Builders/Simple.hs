-- Copyright (c) 2010-2018 Brett Lajzer
-- See LICENSE for license information.

-- | The Simple builder allows you to execute a 'OneToOne' trasformation
-- given a function that generates a command line from the source and
-- target files.
module Dib.Builders.Simple (
  makeSimpleTarget
  ) where

import Dib.Gatherers
import Dib.Types
import Dib.Util

import qualified Data.Text as T
import qualified System.Console.ANSI as ANSI
import qualified System.Directory as D
import System.Process (system)
import System.FilePath as P

buildFunc :: (String -> String -> String) -> SrcTransform -> IO StageResult
buildFunc func (OneToOne source target) = do
  let unpackedTarget = T.unpack target
  let unpackedSource = T.unpack source
  D.createDirectoryIfMissing True $ takeDirectory unpackedTarget
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White]
  putStr "Building: "
  ANSI.setSGR [ANSI.Reset]
  putStrLn $ unpackedSource ++ " -> " ++ unpackedTarget
  let buildCmd = func unpackedSource unpackedTarget
  exitCode <- system buildCmd
  handleExitCode exitCode target buildCmd
buildFunc _ _ = return $ Left "Unexpected SrcTransform"

remapFile :: String -> String -> T.Text -> SrcTransform -> SrcTransform
remapFile src dest ext (OneToOne source _) = OneToOne source $ T.pack $ dest </> makeRelative src (T.unpack (changeExt source ext))
remapFile _ _ _ _ = error "Unhandled SrcTransform"

changeExt :: T.Text -> T.Text -> T.Text
changeExt path = T.append (T.dropWhileEnd (/='.') path)

-- | The 'makeSimpleTarget' function generates a target.
-- It takes a name, source directory, destination directory, destination extension,
-- gather filter, and a function to build the command line.
makeSimpleTarget :: T.Text -> T.Text -> T.Text -> T.Text -> FilterFunc -> [T.Text] -> (String -> String -> String) -> Target
makeSimpleTarget name src dest ext filterFunc extraDeps buildCmdBuilder =
  let stage = Stage name (map $ remapFile (T.unpack src) (T.unpack dest) ext) return extraDeps (buildFunc buildCmdBuilder)
  in Target name (const 0) [] [stage] [makeFileTreeGatherer src filterFunc]
