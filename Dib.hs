module Dib (SrcTransform(OneToOne, OneToMany, ManyToOne, ManyToMany),
            Actionable,
            execAction,
            Rule,
            evalRule,
            Action(Action),
            runAction
            ) where

import Control.Monad
import qualified Data.Map as M
import qualified Data.ByteString as B
import Data.List
import Data.Maybe
import Data.Serialize
import System.Cmd (system)
import System.Directory as D
import System.FilePath
import System.Time as T

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

--TODO: modify this to pass in the real map
runAction :: (Actionable a, Rule r) => Action r a -> [FilePath] -> IO [FilePath]
runAction (Action rule impl) files = let gatheredFiles = evalRule rule files in (filterM (shouldBuildTransform M.empty) gatheredFiles) >>= (execAction impl) >> (return $ extractTargets gatheredFiles)

(<||>) :: IO Bool -> IO Bool -> IO Bool
(<||>) = liftM2 (||)

-- function for filtering transforms based on them already being taken care of
-- TODO: add in something that queries the database
shouldBuildTransform :: M.Map FilePath Integer -> SrcTransform -> IO Bool
shouldBuildTransform m (OneToOne s d) = hasSrcChanged m [s] <||> (liftM not $ D.doesFileExist d)
shouldBuildTransform m (OneToMany s ds) = hasSrcChanged m [s] <||> (liftM (not.and) $ mapM D.doesFileExist ds)
shouldBuildTransform m (ManyToOne ss d) = hasSrcChanged m ss <||> (liftM not $ D.doesFileExist d)
shouldBuildTransform m (ManyToMany ss ds) = hasSrcChanged m ss <||> (liftM (not.and) $ mapM D.doesFileExist ds)

hasSrcChanged :: M.Map FilePath Integer -> [FilePath] -> IO Bool
hasSrcChanged m f = let filesInMap = zip f $ map (flip M.lookup m) f
                        checkTimeStamps _ (_, Nothing) = return True
                        checkTimeStamps b (f, Just s) = (getTimestamp f) >>= (\t -> return $ b || (t /= s))
                    in foldM checkTimeStamps True filesInMap

getTimestamp f = do doesExist <- D.doesFileExist f
                    if doesExist then D.getModificationTime f >>= extractSeconds else return 0
                    where extractSeconds (TOD s p) = return s             

loadDatabase :: IO (M.Map FilePath Integer)
loadDatabase = do fileExists <- D.doesFileExist ".dibdb"
                  fileContents <- if fileExists then B.readFile ".dibdb" else return B.empty
                  return.handleEither $ decode fileContents
                  where handleEither (Left _) = M.empty
                        handleEither (Right a) = a
                        
writeDatabase :: (M.Map FilePath Int) -> IO ()
writeDatabase m = do B.writeFile ".dibdb" $ encode m
