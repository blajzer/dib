-- Copyright (c) 2010-2018 Brett Lajzer
-- See LICENSE for license information.

-- | Module exposing the 'Stage' type and related type wrappers, along with a
-- convenience 'emptyStage'.
module Dib.Stage(
  Stage(Stage),
  InputTransformer,
  DepScanner,
  StageFunc,
  emptyStage
  ) where

import Dib.Types

-- | A stage that does nothing and just passes the 'SrcTransform's through.
emptyStage :: Stage
emptyStage = Stage "empty" id return [] (return.Right)
