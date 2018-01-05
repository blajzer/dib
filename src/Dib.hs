-- Copyright (c) 2010-2018 Brett Lajzer
-- See LICENSE for license information.

-- | = Introduction
-- Dib is a light-weight, forward build system embedded in Haskell.
-- Dib represents the build products as a chain of operations starting at the input.
-- Reverse build systems such as Make and Jam instead attempt to figure out the
-- operations to perform starting at the desired output and tracing back through
-- a set of rules to find the correct input file. Dib has no such notion of "rules"
-- and the general thought process for writing a build script answers the question
-- "I have these files, how do I build them into the thing I want?", versus a reverse
-- build system which answers (recursively) "I want this product, what files do I
-- need to use as input?".
--
-- = Concepts
-- * 'Target' - The most granluar unit of a build. Represents a desired outcome:
--   e.g. an executable, a folder of files, etc... Contains 'Stage's, which do
--   the actual work. Somewhat unfortunately, a 'Target'\'s name is its only identifier
--   in the cache database, so debug/release and multiplatform 'Target' variants
--   should be named accordingly to prevent full-rebuilds when switching between them.
-- * 'Stage' - A portion of a pipeline for transforming input data into output data.
--   These separate major portions of a pipeline: e.g. building source code into
--   object files, linking object files into an executable, copying some data into place.
--   'Target's can have multiple 'Stage's, which are executed in sequence, the output
--   of one is used as the input to the next.
-- * 'Gatherer' - Used to generate the initial input 'SrcTransform's for the first
--   'Stage' of a 'Target'.
-- * 'SrcTransform' - Represents a mapping from input to output. Comes in four varieties:
--   'OneToOne', 'OneToMany', 'ManyToOne', 'ManyToMany'. Some examples: compiling a
--   C source file into an object file initially begins as a 'OneToOne', but is converted
--   into a 'ManyToOne' through dependency scanning (adding the dependencies to the input
--   exploits the internal timestamp database for free). Copying files from one location to
--   another would just be a simple 'OneToOne'. A tool that takes in one file and
--   generates a bunch of output files would use 'OneToMany'.
--
-- = Getting Started
-- Dib is both a library and an executable. The executable exists to cause a rebuild
-- of the build script whenever it changes, and also as a convenience for invoking both
-- the build and execution correctly. It's recommended that it be used for everything
-- except extraordinary use cases. It can also generate an initial build script
-- through the use of @dib --init@. Run the dib executable with no options for more
-- information on the available templates.
--
-- An example of using the C Builder to build an executable called "myProject" with
-- its source code in the "src/" directory is as follows:
--
-- @
-- module Main where
--
-- import Dib
-- import Dib.Builders.C
-- import qualified Data.Text as T
--
-- projectInfo = defaultGCCConfig {
--   outputName = "myProject",
--   targetName = "myProject",
--   srcDir = "src",
--   compileFlags = "",
--   linkFlags = "",
--   outputLocation = ObjAndBinDirs "obj" ".",
--   includeDirs = ["src"]
-- }
--
-- project = makeCTarget projectInfo
-- clean = makeCleanTarget projectInfo
--
-- targets = [project, clean]
--
-- main = dib targets
-- @
--
-- This was generated with @dib --init c myProject gcc src@.
--
-- A build script is expected to declare the available 'Target's and then pass them
-- to the 'dib' function. Only the top-level 'Target's need to be passed to 'dib';
-- it will scrape out the dependencies from there. The first 'Target' in the list
-- is the default 'Target' to build if the dib executable is called with no arguments.
--
-- == Additional Information
-- Arguments can be passed on the command line to the dib executable. These can be
-- retrieved in the build with 'getArgDict'. The user is also free to use environment variables
-- as parameter input.
--
-- The invocation might look like the following: @dib <target> <key>=<value> <key>=<value> ...@.
-- Please note that there are no spaces between the keys and values. Quoted strings are
-- untested and unlikely to work correctly.  The 'Target' is optional, and can appear
-- anywhere in the command. If no 'Target' is specified, the default will be used.
--
--
module Dib (
  SrcTransform(OneToOne, OneToMany, ManyToOne, ManyToMany),
  dib,
  getArgDict,
  addEnvToDict,
  makeArgDictLookupFunc,
  makeArgDictLookupFuncChecked
  ) where

import Dib.Gatherers
import Dib.Target
import Dib.Types
import Control.Concurrent
import Control.Monad
import Control.Monad.State as S
import qualified Data.ByteString as B
import qualified Data.Digest.CRC32 as Hash
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Serialize as Serialize
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified GHC.Conc as GHC
import qualified System.Directory as D
import qualified System.Environment as Env
import Data.Maybe
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Word
import System.IO

databaseFile :: String
databaseFile = ".dib/dibdb"

databaseVersion :: Integer
databaseVersion = 5

-- | The function that should be called to dispatch the build. Takes a list
-- of the top-level (root) 'Target's.
dib :: [Target] -> IO ()
dib targets = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  args <- Env.getArgs
  numProcs <- GHC.getNumProcessors

  -- Validate that we have at least one target
  if null targets
    then putStrLn "ERROR: Invalid configuration, no targets defined."
    else do
      -- Validate targets
      let allTargets = gatherAllTargets targets
      let targetErrors = validateTargets allTargets
      if isJust targetErrors
        then putStrLn $ "ERROR: Invalid targets:\n" ++ fromJust targetErrors
        else do
          let buildArgs = parseArgs args allTargets numProcs
          let selectedTarget = buildTarget buildArgs
          let theTarget = L.find (\(Target name _ _ _ _) -> name == selectedTarget) allTargets

          -- Validate that we're trying to build something that exists
          if isNothing theTarget
            then putStrLn $ "ERROR: Invalid target specified: \"" ++ T.unpack selectedTarget ++ "\""
            else do
              -- load the database
              dbLoadStart <- getCurrentTime
              (tdb, cdb, tcdb) <- loadDatabase
              dbLoadEnd <- getCurrentTime

              -- run the build
              startTime <- getCurrentTime
              let buildState = BuildState buildArgs selectedTarget tdb cdb tcdb Set.empty Map.empty
              (_, s) <- runBuild (runTarget $ fromJust theTarget) buildState
              endTime <- getCurrentTime

              -- save the database
              dbSaveStart <- getCurrentTime
              saveDatabase (getTargetTimestampDB s) (getChecksumDB s) (getTargetChecksumDB s)
              dbSaveEnd <- getCurrentTime

              -- output build stats
              putStrLn $ "DB load/save took " ++ show (diffUTCTime dbLoadEnd dbLoadStart) ++ "/" ++ show (diffUTCTime dbSaveEnd dbSaveStart) ++ " seconds."
              putStrLn $ "Build took " ++ show (diffUTCTime endTime startTime) ++ " seconds."

gatherAllTargetsInternal :: [Target] -> Set.Set Target -> Set.Set Target
gatherAllTargetsInternal (t:ts) s =
  let (recurse, newSet) = if Set.notMember t s then (True, Set.insert t s) else (False, s)
  in if recurse then gatherAllTargetsInternal ts (gatherAllTargetsInternal (getDependencies t) newSet) else gatherAllTargetsInternal ts newSet
gatherAllTargetsInternal [] s = s

gatherAllTargets :: [Target] -> [Target]
gatherAllTargets t =
  let allTargets = Set.toList $ gatherAllTargetsInternal t Set.empty
      targetsMinusInitial = L.filter (\x -> x /= head t) allTargets
  in head t : targetsMinusInitial

validateTargets :: [Target] -> Maybe String
validateTargets ts =
  let targetErrors = L.foldl' (\acc t -> acc ++ validate t) "" ts
      validate (Target name _ _ stages gatherers) = if not (null stages) && null gatherers then T.unpack name ++ ": target requires at least one gatherer since it specifies at least one stage.\n" else ""
  in if targetErrors == "" then Nothing else Just targetErrors

extractVarsFromArgs :: [String] -> ArgDict
extractVarsFromArgs args = L.foldl' extractVarsFromArgsInternal Map.empty $ map (L.break (== '=')) args
  where
    extractVarsFromArgsInternal e (_, []) = e
    extractVarsFromArgsInternal e (a, _:bs) = Map.insert a bs e

-- | Returns the argument dictionary.
getArgDict :: IO ArgDict
getArgDict = do
  args <- Env.getArgs
  return $ extractVarsFromArgs args

-- | Adds all of the variables in the execution environment into the
-- argument dictionary. Allows for make-like variable passing.
addEnvToDict :: ArgDict -> [(String, String)] -> IO ArgDict
addEnvToDict m vars = do
  env <- Env.getEnvironment
  let valuesToAdd = map (\(x, y) -> (x, fromMaybe y $ L.lookup x env)) vars
  return $ L.foldl' (\a (x, y) -> Map.insert x y a) m valuesToAdd

removeVarsFromArgs :: [String] -> [String]
removeVarsFromArgs args = L.foldl' removeVarsFromArgsInternal [] $ map (L.break (== '=')) args
  where
    removeVarsFromArgsInternal e (t, []) = e ++ [t]
    removeVarsFromArgsInternal e (_, _:_) = e

parseArgs :: [String] -> [Target] -> Int -> BuildArgs
parseArgs args targets numJobs =
  let cleanArgs = removeVarsFromArgs args
      argsLen = length cleanArgs
      target = if argsLen > 0 then T.pack.head $ cleanArgs else T.pack.show.head $ targets
  in BuildArgs { buildTarget = target, maxBuildJobs = numJobs }

-- | Makes a function that can be used to look up a value in the argument
-- dictionary, returning a default value if the argument does not exist.
makeArgDictLookupFunc :: String -> String -> ArgDict -> String
makeArgDictLookupFunc arg defVal dict = fromMaybe defVal $ Map.lookup arg dict

-- | Makes a function that can be used to look up a value in the argument
-- dictionary, returning a default value if the argument does not exist, and
-- checking success against a list of valid values.
-- Returns an error string on Left, and success string on Right.
makeArgDictLookupFuncChecked :: String -> String -> [String] -> ArgDict -> Either String String
makeArgDictLookupFuncChecked arg defVal validValues dict =
    let partialResult = makeArgDictLookupFunc arg defVal dict
        result = L.find (== partialResult) validValues
    in maybe (Left $ "ERROR: invalid value \"" ++ partialResult ++ "\" for argument \"" ++ arg ++ "\". Expected one of: [" ++ L.intercalate  ", " validValues ++ "]") Right result

printSeparator :: IO ()
printSeparator = putStrLn "============================================================"

runBuild :: BuildM a -> BuildState -> IO (a, BuildState)
runBuild m = runStateT (runBuildImpl m)

loadDatabase :: IO (TargetTimestampDB, ChecksumDB, TargetChecksumDB)
loadDatabase = do fileExists <- D.doesFileExist databaseFile
                  fileContents <- if fileExists then B.readFile databaseFile else return B.empty
                  return.handleEither $ Serialize.decode fileContents
                  where handleEither (Left _) = (Map.empty, Map.empty, Map.empty)
                        handleEither (Right (v, t, c, tc)) = if v == databaseVersion then (t, c, tc) else (Map.empty, Map.empty, Map.empty)

saveDatabase :: TargetTimestampDB -> ChecksumDB -> TargetChecksumDB -> IO ()
saveDatabase tdb cdb tcdb = B.writeFile databaseFile $ Serialize.encode (databaseVersion, tdb, cdb, tcdb)

getCurrentTargetName :: BuildState -> T.Text
getCurrentTargetName (BuildState _ t _ _ _ _ _) = t

putCurrentTargetName :: BuildState -> T.Text -> BuildState
putCurrentTargetName (BuildState a _ tdb cdb tcdb ts p) t = BuildState a t tdb cdb tcdb ts p

getTargetTimestampDB :: BuildState -> TargetTimestampDB
getTargetTimestampDB (BuildState _ _ tdb _ _ _ _) = tdb

-- | Returns the 'TimestampDB' from the 'BuildState'
getTimestampDB :: BuildState -> TimestampDB
getTimestampDB (BuildState _ t tdb _ _ _ _) = Map.findWithDefault Map.empty t tdb

-- | Puts the 'TimestampDB' back into the 'BuildState'
putTimestampDB :: BuildState -> TimestampDB -> BuildState
putTimestampDB (BuildState a t ftdb cdb tcdb ts p) tdb = BuildState a t (Map.insert t tdb ftdb) cdb tcdb ts p

getChecksumDB :: BuildState -> ChecksumDB
getChecksumDB (BuildState _ _ _ cdb _ _ _) = cdb

putChecksumDB :: BuildState -> ChecksumDB -> BuildState
putChecksumDB (BuildState a t tdb _ tcdb ts p) cdb = BuildState a t tdb cdb tcdb ts p

getTargetChecksumDB :: BuildState -> TargetChecksumDB
getTargetChecksumDB (BuildState _ _ _ _ tcdb _ _) = tcdb

putTargetChecksumDB :: BuildState -> TargetChecksumDB -> BuildState
putTargetChecksumDB (BuildState a t tdb cdb _ ts p) tcdb = BuildState a t tdb cdb tcdb ts p

getUpToDateTargets :: BuildState -> UpToDateTargets
getUpToDateTargets (BuildState _ _ _ _ _ ts _) = ts

putUpToDateTargets :: BuildState -> UpToDateTargets -> BuildState
putUpToDateTargets (BuildState a t tdb cdb tcdb _ p) ts = BuildState a t tdb cdb tcdb ts p

getPendingDBUpdates :: BuildState -> PendingDBUpdates
getPendingDBUpdates (BuildState _ _ _ _ _ _ p) = p

putPendingDBUpdates :: BuildState -> PendingDBUpdates -> BuildState
putPendingDBUpdates (BuildState a t tdb cdb tcdb ts _) = BuildState a t tdb cdb tcdb ts

getMaxBuildJobs :: BuildState -> Int
getMaxBuildJobs (BuildState a _ _ _ _ _ _) = maxBuildJobs a

-- | Returns whether or not a target is up to date, based on the current build state.
targetIsUpToDate :: BuildState -> Target -> Bool
targetIsUpToDate (BuildState _ _ _ _ _ s _) t = Set.member t s

-- | Partitions out up-to-date mappings
partitionMappings :: T.Text -> T.Text -> [SrcTransform] -> [T.Text] -> Bool -> BuildM ([SrcTransform], [SrcTransform])
partitionMappings targetName stageName files extraDeps force = do
  s <- get
  extraDepsChanged <- liftIO $ hasSrcChanged (getTimestampDB s) (ManyToOne extraDeps (T.concat [targetName, "^:^", stageName])) extraDeps
  if force || extraDepsChanged then
      return (files, [])
    else do
      shouldBuild <- liftIO $ mapM (shouldBuildMapping (getTimestampDB s) (getChecksumDB s)) files
      let paired = zip shouldBuild files
      let (a, b) = L.partition fst paired
      return (map snd a, map snd b)

(<||>) :: IO Bool -> IO Bool -> IO Bool
(<||>) = liftM2 (||)

-- function for filtering FileMappings based on them already being taken care of
shouldBuildMapping :: TimestampDB -> ChecksumDB -> SrcTransform -> IO Bool
shouldBuildMapping t c src@(OneToOne s d) = hasSrcChanged t src [s] <||> hasChecksumChanged c [s] [d] <||> fmap not (D.doesFileExist $ T.unpack d)
shouldBuildMapping t c src@(OneToMany s ds) = hasSrcChanged t src [s] <||> hasChecksumChanged c [s] ds  <||> fmap (not.and) (mapM (D.doesFileExist.T.unpack) ds)
shouldBuildMapping t c src@(ManyToOne ss d) = hasSrcChanged t src ss <||> hasChecksumChanged c ss [d]  <||> fmap not (D.doesFileExist $ T.unpack d)
shouldBuildMapping t c src@(ManyToMany ss ds) = hasSrcChanged t src ss <||> hasChecksumChanged c ss ds  <||> fmap (not.and) (mapM (D.doesFileExist.T.unpack) ds)

hashText :: T.Text -> Word32
hashText t = Hash.crc32 $ TE.encodeUtf8 t

hashTransform :: SrcTransform -> [Word32]
hashTransform (OneToOne s d) = [hashText $ T.concat [s, "^^^^", d]]
hashTransform (OneToMany s ds) = [hashText $ T.concat $ s : "^^^^" : L.intersperse ":" ds]
hashTransform (ManyToOne ss d) = map (\s -> hashText $ T.concat [s, "^^^^", d]) ss
hashTransform (ManyToMany ss ds) =
  let destMux = L.intersperse ":" ds
  in map (\s -> hashText $ T.concat $ s : "^^^^" : destMux) ss

hasSrcChanged :: TimestampDB -> SrcTransform -> [T.Text] -> IO Bool
hasSrcChanged m tr f = let filesInMap = zip f $ map (`Map.lookup` m) $ hashTransform tr
                           checkTimeStamps _ (_, Nothing) = return True
                           checkTimeStamps b (file, Just s) = getTimestamp file >>= (\t -> return $ b || (t /= s))
                    in foldM checkTimeStamps False filesInMap

getTimestamp :: T.Text -> IO Integer
getTimestamp f = do
  let unpackedFileName = T.unpack f
  doesExist <- D.doesFileExist unpackedFileName
  if doesExist then D.getModificationTime unpackedFileName >>= extractSeconds else return 0
  where extractSeconds s = return $ (fromIntegral.fromEnum.utcTimeToPOSIXSeconds) s

hasChecksumChanged :: ChecksumDB -> [T.Text] -> [T.Text] -> IO Bool
hasChecksumChanged cdb s d = do
  let (key, cs) = getChecksumPair s d
  let mapVal = Map.lookup key cdb
  return $ compareChecksums mapVal cs
  where compareChecksums (Just mcs) ccs = mcs /= ccs
        compareChecksums Nothing _ = True

getChecksumPair :: [T.Text] -> [T.Text] -> (Word32, Word32)
getChecksumPair s d =
  let joinedSrc = T.concat $ L.intersperse ":" s
      joinedDest = T.concat $ L.intersperse ":" d
  in (hashText joinedDest, hashText joinedSrc)

buildFoldFunc :: StageResults -> Target -> BuildM StageResults
buildFoldFunc l@(Left _) _ = return l
buildFoldFunc (Right _) t@(Target name _ _ _ _) = do
  buildState <- get
  let oldTargetName = getCurrentTargetName buildState
  put $ putCurrentTargetName buildState name
  result <- runTarget t
  newBuildState <- get
  put $ putCurrentTargetName newBuildState oldTargetName
  return result

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

runTarget :: Target -> BuildM StageResults
runTarget t@(Target name _ deps _ _) = do
  buildState <- get
  let outdatedTargets = filter (not.targetIsUpToDate buildState) deps
  depStatus <- foldM buildFoldFunc (Right []) outdatedTargets
  if isRight depStatus then
      runTargetInternal t
    else
      buildFailFunc depStatus name

buildFailFunc :: StageResults -> T.Text -> BuildM StageResults
buildFailFunc (Left err) name = do
  liftIO printSeparator
  liftIO $ putStr $ "ERROR: Error building target \"" ++ T.unpack name ++ "\": "
  liftIO $ putStrLn $ T.unpack err
  return $ Left ""
buildFailFunc (Right _) _ = return $ Left ""

runTargetInternal :: Target -> BuildM StageResults
runTargetInternal t@(Target name hashFunc _ stages gatherers) = do
  buildState <- get
  let tcdb = getTargetChecksumDB buildState
  let checksum = hashFunc t
  let forceRebuild = checksum /= Map.findWithDefault 0 name tcdb
  gatheredFiles <- liftIO $ runGatherers gatherers
  let srcTransforms = map (flip OneToOne "") gatheredFiles
  liftIO $ putStrLn $ "==== Target: \"" ++ T.unpack name ++ "\""
  stageResult <- foldM (stageFoldFunc name) (Right srcTransforms) $ zip stages $ repeat forceRebuild

  -- Update target checksum to prevent future, forced builds
  buildState <- get
  let updatedChecksums = Map.insert name (hashFunc t) $ getTargetChecksumDB buildState
  put $ putTargetChecksumDB buildState updatedChecksums
  if isRight stageResult then targetSuccessFunc t else buildFailFunc stageResult name

targetSuccessFunc :: Target -> BuildM StageResults
targetSuccessFunc t@(Target name hashFunc _ _ _) = do
  buildState <- get
  let updatedTargets = Set.insert t $ getUpToDateTargets buildState
  put $ putUpToDateTargets buildState updatedTargets
  liftIO $ putStrLn $ "Successfully built target \"" ++ T.unpack name ++ "\""
  liftIO $ putStrLn ""
  return $ Right []

stageFoldFunc :: T.Text -> StageResults -> (Stage, Bool) -> BuildM StageResults
stageFoldFunc targetName (Right t) (s, force) = runStage targetName s force t
stageFoldFunc _ l@(Left _) _ = return l

workerThreadFunc :: (SrcTransform -> IO StageResult) -> MVar [SrcTransform] -> MVar (StageResults, [BuildM ()]) -> MVar (StageResults, [BuildM ()]) -> MVar Int -> IO ()
workerThreadFunc sf q r f c = do
  queue <- takeMVar q
  if null queue then do
      putMVar q queue
      count <- takeMVar c
      let newCount = count - 1
      if newCount == 0 then do
          putMVar c newCount
          finalResult <- readMVar r
          putMVar f finalResult
          return ()
        else do
          putMVar c newCount
          return ()
    else do
      let workItem = head queue
      putMVar q (tail queue)
      taskResult <- sf workItem
      let dbThunk = updateDatabase taskResult workItem
      resultAcc <- takeMVar r
      let combine left@(Left _) _ = left
          combine (Right ml) (Right v) = Right (v : ml)
          combine (Right _) (Left v) = Left v
      let newResultAcc = (\(res, thunks) -> (combine res taskResult, dbThunk : thunks)) resultAcc
      putMVar r newResultAcc
      workerThreadFunc sf q r f c

stageHelper :: (SrcTransform -> IO StageResult) -> Int -> [SrcTransform] -> StageResults -> BuildM StageResults
stageHelper f m i r = do
  finalResultMVar <- liftIO newEmptyMVar
  resultMVar <- liftIO $ newMVar (r, []) -- (overall result, database thunks)
  queueMVar <- liftIO $ newMVar i
  threadCountMVar <- liftIO $ newMVar m
  if null i then
      return r
    else do
      liftIO $ replicateM_ m (workerThreadFunc f queueMVar resultMVar finalResultMVar threadCountMVar)
      result <- liftIO $ takeMVar finalResultMVar
      sequence_ $ snd result
      return $ fst result

runStage :: T.Text -> Stage -> Bool -> [SrcTransform] -> BuildM StageResults
runStage targetName s@(Stage name _ _ extraDeps f) force m = do
  liftIO $ putStrLn $ "-- Stage: \"" ++ T.unpack name ++ "\""
  depScannedFiles <- liftIO $ processMappings s m
  (targetsToBuild, upToDateTargets) <- partitionMappings targetName name depScannedFiles extraDeps force
  bs <- get
  result <- stageHelper f (getMaxBuildJobs bs) targetsToBuild (Right $ map transferUpToDateTarget upToDateTargets)
  writePendingDBUpdates
  updateDatabaseExtraDeps targetName name result extraDeps

-- These might not be quite correct. I guessed at what made sense.
transferUpToDateTarget :: SrcTransform -> SrcTransform
transferUpToDateTarget (OneToOne _ d) = OneToOne d ""
transferUpToDateTarget (OneToMany _ ds) = ManyToOne ds ""
transferUpToDateTarget (ManyToOne _ d) = OneToOne d ""
transferUpToDateTarget (ManyToMany _ ds) = ManyToOne ds ""

processMappings :: Stage -> [SrcTransform] -> IO [SrcTransform]
processMappings (Stage _ t d _ _) m = do
  let transMap = t m --transform input-only mappings into input -> output mappings
  mapM d transMap

updateDatabase :: Either l r -> SrcTransform -> BuildM ()
updateDatabase (Left _) _ = return ()
updateDatabase (Right _) src@(OneToOne s d) = updateDatabaseHelper src [s] [d]
updateDatabase (Right _) src@(OneToMany s ds) = updateDatabaseHelper src [s] ds
updateDatabase (Right _) src@(ManyToOne ss d) = updateDatabaseHelper src ss [d]
updateDatabase (Right _) src@(ManyToMany ss ds) = updateDatabaseHelper src ss ds

updateDatabaseHelper :: SrcTransform -> [T.Text] -> [T.Text] -> BuildM ()
updateDatabaseHelper transform srcFiles destFiles = do
  buildstate <- get
  let pdbu = getPendingDBUpdates buildstate
  timestamps <- liftIO $ mapM getTimestamp srcFiles
  let filteredResults = filter (\(_, v) -> v /= 0) $ zip (hashTransform transform) timestamps
  let updatedPDBU = L.foldl' (\m (k, v) -> Map.insert k v m) pdbu filteredResults
  let cdb = getChecksumDB buildstate
  let (key, cs) = getChecksumPair srcFiles destFiles
  let updatedCDB = Map.insert key cs cdb
  put $ putChecksumDB (putPendingDBUpdates buildstate updatedPDBU) updatedCDB
  return ()

updateDatabaseExtraDeps :: T.Text -> T.Text -> StageResults -> [T.Text] -> BuildM StageResults
updateDatabaseExtraDeps _ _ result@(Left _) _ = return result
updateDatabaseExtraDeps targetName stageName result@(Right _) deps = do
  buildstate <- get
  let tdb = getTimestampDB buildstate
  timestamps <- liftIO $ mapM getTimestamp deps
  let filteredResults = filter (\(_, v) -> v /= 0) $ zip (hashTransform $ ManyToOne deps (T.concat [targetName, "^:^", stageName])) timestamps
  let updatedTDB = L.foldl' (\m (k, v) -> Map.insert k v m) tdb filteredResults
  put $ putTimestampDB buildstate updatedTDB
  return result

writePendingDBUpdates :: BuildM ()
writePendingDBUpdates = do
  buildstate <- get
  let tdb = getTimestampDB buildstate
  let pdbu = getPendingDBUpdates buildstate
  let updatedTDB = Map.union pdbu tdb
  put $ putPendingDBUpdates (putTimestampDB buildstate updatedTDB) Map.empty
  return ()
