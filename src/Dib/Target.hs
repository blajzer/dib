module Dib.Target(
  Target(Target),
  addDependency,
  makePhonyTarget
  ) where

import Dib.Types

import qualified Data.Text as T

addDependency :: Target -> Target -> Target
addDependency newDep (Target name deps stages gatherers) = Target name (newDep : deps) stages gatherers

makePhonyTarget :: T.Text -> [Target] -> Target
makePhonyTarget name deps = Target name deps [] []

