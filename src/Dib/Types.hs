{-# LANGUAGE GeneralizedNewtypeDeriving, ExistentialQuantification #-}
module Dib.Types where

import Control.Monad.State as S
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

type BuildArgs = [T.Text]
type TimestampDB = Map.Map T.Text Integer
type UpToDateTargets = Set.Set Target

data BuildState = BuildState BuildArgs TimestampDB UpToDateTargets

newtype BuildM a = BuildMImpl {
  runBuildImpl :: S.StateT BuildState IO a
  } deriving (Monad, MonadIO, MonadState BuildState)

-- | Data type to expressing mapping of input files to output files
data SrcTransform = OneToOne T.Text T.Text
                 | OneToMany T.Text [T.Text]
                 | ManyToOne [T.Text] T.Text
                 | ManyToMany [T.Text] [T.Text]

type InputTransformer = (SrcTransform -> SrcTransform)
type DepScanner = (SrcTransform -> IO SrcTransform)
type StageFunc = (SrcTransform -> IO (Either SrcTransform T.Text))

data Stage = Stage T.Text InputTransformer DepScanner StageFunc

data Target = Target T.Text [Target] [Stage] [Gatherer]

instance Eq Stage where
  (==) (Stage n _ _ _) (Stage n2 _ _ _) = n Prelude.== n2

instance Eq Target where
  (==) (Target n _ _ _) (Target n2 _ _ _) = n Prelude.== n2
  
instance Ord Target where
  compare (Target n _ _ _) (Target n2 _ _ _) = compare n n2
  
class GatherStrategy a where
  gather :: a -> IO [T.Text]

data Gatherer = forall s . GatherStrategy s => Gatherer s

type FilterFunc = (T.Text -> Bool)

data SingleFileGatherer = SingleFileGatherer T.Text
data DirectoryGatherer = DirectoryGatherer T.Text FilterFunc
data FileTreeGatherer = FileTreeGatherer T.Text FilterFunc
  
