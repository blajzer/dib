-- Copyright (c) 2010-2016 Brett Lajzer
-- See LICENSE for license information.

-- | Module that exposes the 'Target' data type and a handful of convenience functions
-- for dealing with 'Target's.
module Dib.Target(
  Target(Target),
  addDependency,
  addDependencies,
  getDependencies,
  makePhonyTarget,
  makeCommandTarget,
  targetDepChecksum
  ) where

import Dib.Types
import Dib.Gatherers

import Data.Word

import qualified Data.Digest.CRC32 as Hash
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | Adds a dependency on another 'Target'.
addDependency :: Target -> Target -> Target
addDependency (Target name checksum deps stages gatherers) newDep = Target name checksum (newDep : deps) stages gatherers

-- | Adds a dependency on a list of 'Target's.
addDependencies :: Target -> [Target] -> Target
addDependencies (Target name checksum deps stages gatherers) newDeps = Target name checksum (newDeps ++ deps) stages gatherers

-- | Gets dependencies of a 'Target'.
getDependencies :: Target -> [Target]
getDependencies (Target _ _ deps _ _) = deps

-- | Makes a 'Target' that doesn't build anything but can be used as a meta
-- 'Target', i.e. the "all" target in make.
makePhonyTarget :: T.Text -> [Target] -> Target
makePhonyTarget name deps = Target name (\_ -> 0) deps [] []

-- | Makes a 'Target' that runs an arbitrary 'IO' action.
makeCommandTarget :: T.Text -> [Target] -> IO () -> Target
makeCommandTarget name deps command = Target name (\_ -> 0) deps [] [makeCommandGatherer command]

-- | Computes a checksum from the direct dependencies of a target
targetDepChecksum :: Target -> Word32
targetDepChecksum (Target _ _ deps _ _) = Hash.crc32 $ TE.encodeUtf8 $ T.intercalate "^" $ map (\(Target n _ _ _ _) -> n) deps
