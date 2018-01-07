{-# LANGUAGE GeneralizedNewtypeDeriving, ExistentialQuantification, KindSignatures, FlexibleContexts #-}
-- Copyright (c) 2010-2018 Brett Lajzer
-- See LICENSE for license information.

-- | C dependency scanner. Runs a stripped-down pre-processor to scan for
-- include files (recursively).
module Dib.Scanners.CDepScanner (
  cDepScanner
  ) where

import Dib.Types
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text as T
import qualified System.Directory as Dir
import qualified System.FilePath as F
import Control.Applicative()
import Control.Monad.State.Lazy

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
  currentDeps :: S.Set Dependency,
  searchPaths :: [String] }
  deriving (Show)

newtype DepGatherer a = DepGatherer {
  runDepGatherer :: StateT ParseState IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadState ParseState)

removeCR :: String -> String
removeCR = filter (/= '\r')

removeLeadingWS :: String -> String
removeLeadingWS = dropWhile (\x -> x == ' ' || x == '\t')

removeBlockComment :: String -> String
removeBlockComment ('*':'/':xs) = removeComments xs
removeBlockComment (_:xs) = removeBlockComment xs
removeBlockComment [] = error "Unterminated block comment."

removeLineComment :: String -> String
removeLineComment ('\n':xs) = removeComments xs
removeLineComment (_:xs) = removeLineComment xs
removeLineComment [] = []

processCharLiteral :: String -> String
processCharLiteral ('\\':x:'\'':xs) = '\\' : x : '\'' : removeComments xs
processCharLiteral (x:'\'':xs) = x : '\'' : removeComments xs
processCharLiteral (x:xs) = x : removeComments xs
processCharLiteral [] = []

processStringLiteral :: String -> String
processStringLiteral ('\\':'"':xs) = '\\' : '"' : processStringLiteral xs
processStringLiteral ('"':xs) = '"' : removeComments xs
processStringLiteral (x:xs) = x : processStringLiteral xs
processStringLiteral [] = error "Unterminated string literal."

processDirective :: String -> String
processDirective ('\\':'\n':xs) = '\\' : '\n' : processDirective xs
processDirective ('\n':xs) = '\n' : removeComments xs
processDirective (x:xs) = x : processDirective xs
processDirective [] = []

removeComments :: String -> String
removeComments ('#':xs) = '#' : processDirective xs
removeComments ('/':'*':xs) = removeBlockComment xs
removeComments ('/':'/':xs) = removeLineComment xs
removeComments ('\'':xs) = '\'' : processCharLiteral xs
removeComments ('"':xs) = '"' : processStringLiteral xs
removeComments (x:xs) = x : removeComments xs
removeComments [] = []

filterBlank :: [String] -> [String]
filterBlank = filter (\x -> x /= "\n" && x /= [])

extractIncludes :: [String] -> [String]
extractIncludes = filter (\x -> "#include" == takeWhile (/= ' ') x)

dequoteInclude :: String -> String
dequoteInclude s =
  let endPortion = dropWhile (\x -> x /= '\"' && x /= '<') s
      endLen = length endPortion
  in if endLen > 0 then takeWhile (\x -> x /= '\"' && x /= '>') $ tail endPortion else []

-- intial pass, removes comments and leading whitespace, then filters out extra lines
pass1 :: String -> [String]
pass1 s = filterBlank $ map removeLeadingWS $ lines $ removeComments (removeCR s)

-- second pass, cleans up includes
pass2 :: [String] -> [String]
pass2 l = filterBlank $ map dequoteInclude $ extractIncludes l

gatherDependencies :: String -> [String]
gatherDependencies = pass2.pass1

possibleFilenames :: FilePath -> [FilePath] -> [FilePath]
possibleFilenames file = map (\p -> F.normalise $ F.combine p file)

pathToDependency :: FilePath -> Dependency
pathToDependency path = Dependency (F.takeFileName path) path

spider :: forall (m :: * -> *).(MonadIO m, MonadState ParseState m) => String -> m ()
spider file = do
  s <- get
  paths <- filterM (includeFilter $ currentDeps s) $ possibleFilenames file (searchPaths s)
  spiderHelper paths
  return ()
    where
      includeFilter deps f = do
        exists <- liftIO $ Dir.doesFileExist f
        return $ exists && not (S.member (pathToDependency f) deps)

spiderHelper :: forall (m :: * -> *).(MonadIO m, MonadState ParseState m) => [FilePath] -> m ()
spiderHelper [] =  return ()
spiderHelper (file:_) = do
  c <- liftIO $ readFile file
  let deps = gatherDependencies c
  s <- get
  put $ s {currentDeps = S.insert (pathToDependency file) (currentDeps s) }
  mapM_ spider deps
  return ()

spiderLauncher :: forall (m :: * -> *).(MonadIO m, MonadState ParseState m) => FilePath -> m ()
spiderLauncher file = do
  c <- liftIO $ readFile file
  let deps = gatherDependencies c
  mapM_ spider deps
  return ()

getDepsForFile :: [FilePath] -> FilePath -> IO [T.Text]
getDepsForFile includeDirs file = do
  (_, s) <- runStateT (runDepGatherer $ spiderLauncher file) PS {currentDeps=S.empty, searchPaths=[F.dropFileName file, "."] ++ includeDirs }
  return $ L.sort $ map (T.pack.getPathFromDep) (S.toList (currentDeps s))

-- | Takes in a list of include directories, extra dependencies, a 'SrcTransform',
-- and returns a new 'SrcTransform' with the dependencies injected into the source
-- side.
cDepScanner :: [FilePath] -> SrcTransform -> IO SrcTransform
cDepScanner includeDirs (OneToOne i o) = getDepsForFile includeDirs (T.unpack i) >>= \d -> return $ ManyToOne (i:d) o
cDepScanner includeDirs (OneToMany i o) = getDepsForFile includeDirs (T.unpack i) >>= \d -> return $ ManyToMany (i:d) o
cDepScanner _ _ = error "Unimplemented. Implement this if it is a valid relationship."
