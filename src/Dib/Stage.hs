module Dib.Stage(
  Stage(Stage),
  InputTransformer,
  DepScanner,
  StageFunc,
  emptyStage
  ) where

import Dib.Types

emptyStage :: Stage
emptyStage = Stage "empty" id return (return.Left)
