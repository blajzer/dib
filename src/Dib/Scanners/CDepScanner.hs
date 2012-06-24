{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Dib.Scanners.CDepScanner (
  cDepScanner
  ) where

import Dib.Types
import qualified Data.Set as S
import qualified Data.Text as T
import qualified System.Directory as Dir
import qualified System.FilePath as F
import Control.Monad.State.Lazy

--TODO: move to another module. Dib.Types?
--                    Filename, path
data Dependency = Dependency String String
  deriving (Show)
  
instance Eq Dependency where
  (Dependency f1 _) == (Dependency f2 _) = f1 == f2
  
instance Ord Dependency where
  compare (Dependency f1 _) (Dependency f2 _) = compare f1 f2

getPathFromDep :: Dependency -> String
getPathFromDep (Dependency _ path) = path

--    already read includes, list of include paths
data ParseState = PS {
  currentDeps :: (S.Set Dependency),
  searchPaths :: [String] }
  deriving (Show)

newtype DepGatherer a = DepGatherer {
  runDepGatherer :: StateT ParseState IO a
  } deriving (Monad, MonadIO, MonadState ParseState)

removeCR :: String -> String
removeCR = filter (/= '\r')

removeLeadingWS :: String -> String
removeLeadingWS = dropWhile (\x -> x == ' ' || x == '\t')

removeTrailingWS :: String -> String
removeTrailingWS = reverse.removeLeadingWS.reverse

trimWS :: String -> String
trimWS = removeLeadingWS.removeTrailingWS

-- input string -> block comment -> line comment -> output string
removeComments :: String -> Bool -> Bool -> String
removeComments ('/':'*':xs) False False = removeComments xs True False
removeComments ('/':'/':xs) False False = removeComments xs False True
removeComments ('\n':xs) False True = '\n' : removeComments xs False False
removeComments ('*':'/':xs) True False = removeComments xs False False
removeComments (_:xs) True False = removeComments xs True False
removeComments (_:xs) False True = removeComments xs False True
removeComments (x:xs) False False = x : removeComments xs False False
removeComments [] False False = []
removeComments [] False True = []
removeComments [] True False = error "Unterminated block comment."
removeComments _ _ _ = error "Undefined condition encountered."

filterBlank :: [String] -> [String]
filterBlank = filter (\x -> x /= ['\n'] && x /= [])

extractIncludes :: [String] -> [String]
extractIncludes = filter (\x -> "#include" == (takeWhile (/= ' ') x))

dequoteInclude :: String -> String
dequoteInclude s = 
  let endPortion = dropWhile (\x -> x /= '\"' && x /= '<') s
      endLen = length endPortion
  in if endLen > 0 then takeWhile (\x -> x /= '\"' && x /= '>') $ tail $ endPortion else []

-- intial pass, removes comments and leading whitespace, then filters out extra lines
pass1 :: String -> [String]
pass1 s = filterBlank $ map removeLeadingWS $ lines $ removeComments (removeCR s) False False

-- second pass, cleans up includes
pass2 :: [String] -> [String]
pass2 l = filterBlank $ map dequoteInclude $ extractIncludes l

gatherDependencies :: String -> [String]
gatherDependencies = pass2.pass1

possibleFilenames :: FilePath -> [FilePath] -> [FilePath]
possibleFilenames file searchPaths = map (\p -> F.normalise $ F.combine p file) searchPaths

pathToDependency :: FilePath -> Dependency
pathToDependency path = Dependency (F.takeFileName path) path

spider file = do
  s <- get
  paths <- filterM (includeFilter $ currentDeps s) $ possibleFilenames file (searchPaths s) 
  spiderHelper paths
  return ()
    where
      includeFilter deps file = do
        exists <- liftIO $ Dir.doesFileExist file
        return $ exists && (not $ S.member (pathToDependency file) deps)

spiderHelper [] =  return ()
spiderHelper (file:_) = do
  c <- liftIO $ readFile file
  let deps = gatherDependencies c
  s <- get
  put $ s {currentDeps = S.insert (pathToDependency file) (currentDeps s) }
  _ <- mapM spider deps
  s <- get
  put $ s {currentDeps = S.union (currentDeps s) (S.fromList $ map pathToDependency deps) }
  return ()

spiderLauncher file = do
  c <- liftIO $ readFile file
  let deps = gatherDependencies c
  _ <- mapM spider deps
  s <- get
  put $ s {currentDeps = S.union (currentDeps s) (S.fromList $ map pathToDependency deps) }
  return ()

getDepsForFile :: FilePath -> IO [T.Text]
getDepsForFile file = do
  (_, s) <- runStateT (runDepGatherer $ spiderLauncher file) (PS {currentDeps=S.empty, searchPaths=["."] })
  return $ map (T.pack) $ map getPathFromDep $ S.toList (currentDeps s)

cDepScanner :: SrcTransform -> IO SrcTransform
cDepScanner (OneToOne i o) = getDepsForFile (T.unpack i) >>= \d -> return $ ManyToOne (i:d) o
cDepScanner (OneToMany i o) = getDepsForFile (T.unpack i) >>= \d -> return $ ManyToMany (i:d) o
cDepScanner _ = error "Unimplemented. Implement this if it is a valid relationship."

