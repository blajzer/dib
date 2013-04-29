-- | Module that exposes the 'Target' data type and a handful of convenience functions
-- for dealing with 'Target's.
module Dib.Target(
  Target(Target),
  addDependency,
  addDependencies,
  makePhonyTarget,
  makeCommandTarget
  ) where

import Dib.Types
import Dib.Gatherers

import qualified Data.Text as T

-- | Adds a dependency on another 'Target'.
addDependency :: Target -> Target -> Target
addDependency (Target name deps stages gatherers) newDep = Target name (newDep : deps) stages gatherers

-- | Adds a a dependency on a list of 'Target's.
addDependencies :: Target -> [Target] -> Target
addDependencies (Target name deps stages gatherers) newDeps = Target name (newDeps ++ deps) stages gatherers

-- | Makes a 'Target' that doesn't build anything but can be used as a meta
-- 'Target', i.e. the "all" target in make.
makePhonyTarget :: T.Text -> [Target] -> Target
makePhonyTarget name deps = Target name deps [] []

-- | Makes a 'Target' that runs an arbitrary 'IO' action.
makeCommandTarget :: T.Text -> [Target] -> IO () -> Target
makeCommandTarget name deps command = Target name deps [] [makeCommandGatherer command]

