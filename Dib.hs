module Dib (SrcTransform(OneToOne, OneToMany, ManyToOne, ManyToMany),
            Actionable,
            execAction,
            Rule,
            evalRule,
            Action,
            runAction
            ) where

import Control.Monad
import Data.Array
import Data.List
import Data.Maybe
import System.Cmd (system)
import System.Directory as D
import System.FilePath

-- Data type for mapping between src and destination files
data SrcTransform = OneToOne FilePath FilePath
                  | OneToMany FilePath [FilePath]
                  | ManyToOne [FilePath] FilePath
                  | ManyToMany [FilePath] [FilePath]
                  deriving (Show)

-- extracts a list of targets from a list of SrcTransforms
extractTargets xs = foldl' f [] xs
    where f targets (OneToOne s d) = targets ++ [d]
          f targets (OneToMany s d) = targets ++ d
          f targets (ManyToOne s d) = targets ++ [d]
          f targets (ManyToMany s d) = targets ++ d
                                            
class Actionable a where
    execAction :: a -> [SrcTransform] -> IO ()

class Rule r where
    evalRule :: r -> [FilePath] -> [SrcTransform]

-- Actions have a Rule that groups files into a set of transformations
--  and an Actionable that processes these transformations
data (Actionable a, Rule r) => Action r a = Action r a

runAction :: (Actionable a, Rule r) => Action r a -> [FilePath] -> IO [FilePath]
runAction (Action rule impl) files = let gatheredFiles = evalRule rule files in (filterM shouldBuildTransform gatheredFiles) >>= (execAction impl) >> (return $ extractTargets gatheredFiles)

--monadic Boolean operators
mNot :: IO Bool -> IO Bool
mNot = liftM not
(<&&>) :: IO Bool -> IO Bool -> IO Bool
(<&&>) = liftM2 (&&)

-- function for filtering transforms based on them already being taken care of
-- TODO: add in something that queries the database
{-# NOINLINE shouldBuildTransform #-}
shouldBuildTransform :: SrcTransform -> IO Bool
shouldBuildTransform (OneToOne s d) = liftM not $ D.doesFileExist d
shouldBuildTransform (OneToMany s ds) = liftM (not.and) $ mapM D.doesFileExist ds
shouldBuildTransform (ManyToOne ss d) = liftM not $ D.doesFileExist d
shouldBuildTransform (ManyToMany ss ds) = liftM (not.and) $ mapM D.doesFileExist ds
