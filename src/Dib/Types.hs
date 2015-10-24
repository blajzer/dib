{-# LANGUAGE GeneralizedNewtypeDeriving, ExistentialQuantification, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Various types used in dib. Due to certain circumstances, this module does
-- not export any of these types directly; other modules do.
module Dib.Types where

import Control.Applicative
import Control.Monad.State as S
import qualified Data.Map as Map
import qualified Data.Serialize as Serialize
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Word

-- | Type wrapper for the timestamp database.
type TimestampDB = Map.Map T.Text Integer
-- | Type wrapper for the target timestamp database.
type TargetTimestampDB = Map.Map T.Text TimestampDB
-- | Type wrapper for the checksum database.
type ChecksumDB = Map.Map T.Text Word32
-- | Type wrapper for the target checksum database.
type TargetChecksumDB = Map.Map T.Text Word32
-- | Type wrapper for the set of currently up-to-date 'Target's.
type UpToDateTargets = Set.Set Target
-- | Type wrapper for the list of database updates that will happen after
-- a successful 'Target' build.
type PendingDBUpdates = Map.Map T.Text Integer
-- | Type wrapper for the dictionary of arguments that are extracted from the
-- command line.
type ArgDict = Map.Map String String

-- | Internal type that contains information state used by the build.
data BuildState = BuildState BuildArgs T.Text TargetTimestampDB ChecksumDB TargetChecksumDB UpToDateTargets PendingDBUpdates

-- | Configuration arguments used by the build
data BuildArgs = BuildArgs {
  -- | The target to build.
  buildTarget :: T.Text,
  -- | The maximum number of build jobs to spawn.
  maxBuildJobs :: Int
  }

-- | Newtype wrapper for the build monad transformer stack.
newtype BuildM a = BuildMImpl {
  runBuildImpl :: S.StateT BuildState IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadState BuildState)

-- | Data type for expressing mapping of input files to output files
data SrcTransform =
  -- | One input to one output file.
  OneToOne T.Text T.Text
  -- | One input file to many output files.
  | OneToMany T.Text [T.Text]
  -- | Many input files to one output file.
  | ManyToOne [T.Text] T.Text
  -- | Many input to many output files.
  | ManyToMany [T.Text] [T.Text]
  deriving (Show)

-- | Type wrapper for functions transforming a list of 'SrcTransform's into a
-- list of 'SrcTransform's suitable for passing into the 'DepScanner'.
type InputTransformer = ([SrcTransform] -> [SrcTransform])

-- | Type wrapper for a function that takes a 'SrcTransform' and produces
-- a 'SrcTransform' with dependencies included in the input.
type DepScanner = (SrcTransform -> IO SrcTransform)

-- | Type wrapper for a function that given a 'SrcTransform', produces either
-- a 'SrcTransform' containing the output files in the input, or an error.
type StageFunc = (SrcTransform -> IO (Either SrcTransform T.Text))

-- | Type describing a build stage.
-- Takes a name, an 'InputTransformer', 'DepScanner', additional dependencies, and the builder function.
data Stage = Stage T.Text InputTransformer DepScanner [T.Text] StageFunc

-- | Type wrapper for a function that given a Target produces a checksum.
type ChecksumFunc = (Target -> Word32)

-- | Describes a build target.
-- Takes a name, checksum, list of dependencies, list of 'Stage's, and a list of 'Gatherer's.
data Target = Target T.Text ChecksumFunc [Target] [Stage] [Gatherer]

instance Show Target where
  show (Target t _ _ _ _) = T.unpack t

instance Eq Stage where
  (==) (Stage n _ _ _ _) (Stage n2 _ _ _ _) = n Prelude.== n2

instance Eq Target where
  (==) (Target n _ _ _ _) (Target n2 _ _ _ _) = n Prelude.== n2

instance Ord Target where
  compare (Target n _ _ _ _) (Target n2 _ _ _ _) = compare n n2

-- | Typeclass representing data types that can be used to collect files
-- for inout into a target.
class GatherStrategy a where
  -- | Function that given the strategy, will produce a list of file paths.
  gather :: a -> IO [T.Text]

-- | Existential data type for wrapping 'GatherStrategy's so they can be used
-- as a uniform type.
data Gatherer = forall s . GatherStrategy s => Gatherer s

-- | Type wrapper for a function to determine if a file should be returned by
-- a 'Gatherer'.
type FilterFunc = (T.Text -> Bool)

-- | 'Gatherer' that will return exactly one file.
data SingleFileGatherer = SingleFileGatherer T.Text

-- | 'Gatherer' that will return all files in a given directory (but not its
-- subdirectories) that pass the filter.
data DirectoryGatherer = DirectoryGatherer T.Text FilterFunc

-- | 'Gatherer' that will return all files in a given directory tree that pass
-- the filter.
data FileTreeGatherer = FileTreeGatherer T.Text FilterFunc

-- | 'Gatherer' that will run a command and return an empty list.
-- Useful for making clean 'Target's. Use sparingly.
data CommandGatherer = CommandGatherer (IO ())

instance Serialize.Serialize T.Text where
  put s = Serialize.putListOf Serialize.put $ T.unpack s
  get = liftM T.pack $ Serialize.getListOf Serialize.get

