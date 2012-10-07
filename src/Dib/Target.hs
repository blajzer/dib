module Dib.Target(
  Target(Target),
  addDependency,
  addDependencies,
  makePhonyTarget
  ) where

import Dib.Types

import qualified Data.Text as T

addDependency :: Target -> Target -> Target
addDependency (Target name deps stages gatherers) newDep = Target name (newDep : deps) stages gatherers

addDependencies :: Target -> [Target] -> Target
addDependencies (Target name deps stages gatherers) newDeps = Target name (newDeps ++ deps) stages gatherers

makePhonyTarget :: T.Text -> [Target] -> Target
makePhonyTarget name deps = Target name deps [] []

