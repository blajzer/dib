-- | Dib is intended to be a replacement for GNU make which is both easier to
--   work with and more flexible. The aim is to make simple tasks simple to describe.
--
--   The initial motivation was that the proverbial \"one line makefile\" should work for
--   projects that are more complex than the trivial case. Secondary motivation was
--   that a build system shouldn't have its own made up syntax. In the case of
--   both make and Jam, their syntax only cripples the user's ability to express
--   the task at hand.
--
--   Build scripts in Dib are actually programs that import the "Dib" module and
--   call its functions.
--
--   TODO: insert example script here
module Dib (SrcTransform(OneToOne, OneToMany, ManyToOne, ManyToMany),
            Actionable(generateActionCmd),
            Rule(evalRule),
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

-- | A representation of a transformation from a given set of source files to
--   a set of destination files. These are processed by actions to make the
--   transformation happen.
--
--   As a simple example, C source code would be represented as a 'OneToOne' of the .c to .o files,
--   and the link step would be a 'ManyToOne' of the .o's to the final executable.
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
          
-- extracts a list of sources from a list of SrcTransforms
extractSrcs xs = foldl' f [] xs
    where f srcs (OneToOne s d) = srcs ++ [s]
          f srcs (OneToMany s d) = srcs ++ [s]
          f srcs (ManyToOne s d) = srcs ++ s
          f srcs (ManyToMany s d) = srcs ++ s

-- | Actionable is the interface through which actions affect the transformations
--   described by the 'SrcTransform's.                                            
class Actionable a where
    --execAction :: a -> [SrcTransform] -> IO ()
    -- | Given a 'SrcTransform', this generates a command to be executed.
    generateActionCmd :: a -> SrcTransform -> String

-- | A Rule takes a list of 'FilePath's and produces a list of 'SrcTransform's.
--   One execution of the rule must take all files and produce all transforms
--   that an action will be executing on.
class Rule r where
    evalRule :: r -> [FilePath] -> [SrcTransform]

-- | Actions have a 'Rule' that groups files into a set of transformations
--  and an 'Actionable' that processes these transformations
data (Actionable a, Rule r) => Action r a = Action r a 

-- | Runs an action on a list of input files.
runAction :: (Actionable a, Rule r) => Action r a -> [FilePath] -> IO [FilePath]
runAction (Action rule impl) files = do
    let gatheredFiles = evalRule rule files
    let execAction a targets = mapM_ (\x -> putStrLn x >> system x) $ map (generateActionCmd a) targets 
    db <- loadDatabase
    filteredTargets <- filterM (shouldBuildTransform db) gatheredFiles
    execAction impl filteredTargets
    newDb <- updateDBFromTargets db $ extractSrcs filteredTargets
    writeDatabase newDb
    return $ extractTargets gatheredFiles

updateDBFromTargets m targets = foldM foldFunc m targets
    where foldFunc m f = do timeStamp <- getTimestamp f
                            return $ M.insert f timeStamp m   

(<||>) :: IO Bool -> IO Bool -> IO Bool
(<||>) = liftM2 (||)

-- function for filtering transforms based on them already being taken care of
shouldBuildTransform :: M.Map FilePath Integer -> SrcTransform -> IO Bool
shouldBuildTransform m (OneToOne s d) = hasSrcChanged m [s] <||> liftM not (D.doesFileExist d)
shouldBuildTransform m (OneToMany s ds) = hasSrcChanged m [s] <||> liftM (not.and) (mapM D.doesFileExist ds)
shouldBuildTransform m (ManyToOne ss d) = hasSrcChanged m ss <||> liftM not (D.doesFileExist d)
shouldBuildTransform m (ManyToMany ss ds) = hasSrcChanged m ss <||> liftM (not.and) (mapM D.doesFileExist ds)

hasSrcChanged :: M.Map FilePath Integer -> [FilePath] -> IO Bool
hasSrcChanged m f = let filesInMap = zip f $ map (flip M.lookup m) f
                        checkTimeStamps _ (_, Nothing) = return True
                        checkTimeStamps b (f, Just s) = getTimestamp f >>= (\t -> return $ b || (t /= s))
                    in foldM checkTimeStamps False filesInMap

getTimestamp f = do doesExist <- D.doesFileExist f
                    if doesExist then D.getModificationTime f >>= extractSeconds else return 0
                    where extractSeconds (TOD s p) = return s             

loadDatabase :: IO (M.Map FilePath Integer)
loadDatabase = do fileExists <- D.doesFileExist ".dibdb"
                  fileContents <- if fileExists then B.readFile ".dibdb" else return B.empty
                  return.handleEither $ decode fileContents
                  where handleEither (Left _) = M.empty
                        handleEither (Right a) = a
                        
writeDatabase :: M.Map FilePath Integer -> IO ()
writeDatabase m = B.writeFile ".dibdb" $ encode m
