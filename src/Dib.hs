-- | TODO: talk about the point of the project and give some examples.
module Dib (
  SrcTransform(OneToOne, OneToMany, ManyToOne, ManyToMany),
  BuildState,
  dib,
  filterMappings,
  getTimestampDB,
  putTimestampDB,
  targetIsUpToDate,
  getArgDict,
  addEnvToDict,
  makeArgDictLookupFunc
  ) where

import Dib.Gatherers
import Dib.Types
import Control.Concurrent
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
import Data.Time.Clock.POSIX
import Data.Word

databaseFile :: String
databaseFile = ".dib/dibdb"

databaseVersion :: Integer
databaseVersion = 3

-- | The function that should be called to dispatch the build. Takes a list
-- of 'Target's.
dib :: [Target] -> IO ()
dib targets = do
  args <- Env.getArgs
  numProcs <- GHC.getNumProcessors
  let buildArgs = parseArgs args targets numProcs
  let selectedTarget = buildTarget buildArgs
  let theTarget = L.find (\(Target name _ _ _ _) -> name == selectedTarget) targets
  if isNothing theTarget
    then putStrLn $ "ERROR: Invalid target specified: \"" ++ T.unpack selectedTarget ++ "\"" else
    do
      (tdb, cdb, tcdb) <- loadDatabase
      (_, s) <- runBuild runTarget (fromJust theTarget) (BuildState buildArgs selectedTarget tdb cdb tcdb Set.empty Map.empty)
      saveDatabase (getTargetTimestampDB s) (getChecksumDB s) (getTargetChecksumDB s)
      return ()

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

parseArgs :: [String] -> [Target] -> Int -> BuildArgs
parseArgs args targets numJobs =
  let argsLen = length args
      target = if argsLen > 0 then T.pack.head $ args else T.pack.show.head $ targets
  in BuildArgs { buildTarget = target, maxBuildJobs = numJobs }

-- | Makes a function that can be used to look up a value in the argument
-- dictionary, returning a default value if the argument does not exist.
makeArgDictLookupFunc :: String -> String -> ArgDict -> String
makeArgDictLookupFunc arg defVal dict = fromMaybe defVal $ Map.lookup arg dict

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

-- | Filters out up-to-date mappings
filterMappings :: [SrcTransform] -> BuildM [SrcTransform]
filterMappings files = get >>= \(BuildState _ t tdb cdb _ _ _) -> liftIO $ filterM (shouldBuildMapping (Map.findWithDefault Map.empty t tdb) cdb) files

-- | Partitions out up-to-date mappings
partitionMappings :: [SrcTransform] -> [T.Text] -> Bool -> BuildM ([SrcTransform], [SrcTransform])
partitionMappings files extraDeps force = do
  s <- get
  extraDepsChanged <- liftIO $ hasSrcChanged (getTimestampDB s) extraDeps
  if force || extraDepsChanged then do
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
shouldBuildMapping t c (OneToOne s d) = hasSrcChanged t [s] <||> hasChecksumChanged c [s] [d] <||> liftM not (D.doesFileExist $ T.unpack d)
shouldBuildMapping t c (OneToMany s ds) = hasSrcChanged t [s] <||> hasChecksumChanged c [s] ds  <||> liftM (not.and) (mapM (D.doesFileExist.T.unpack) ds)
shouldBuildMapping t c (ManyToOne ss d) = hasSrcChanged t ss <||> hasChecksumChanged c ss [d]  <||> liftM not (D.doesFileExist $ T.unpack d)
shouldBuildMapping t c (ManyToMany ss ds) = hasSrcChanged t ss <||> hasChecksumChanged c ss ds  <||> liftM (not.and) (mapM (D.doesFileExist.T.unpack) ds)

hasSrcChanged :: TimestampDB -> [T.Text] -> IO Bool
hasSrcChanged m f = let filesInMap = zip f $ map (`Map.lookup` m) f
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

getChecksumPair :: [T.Text] -> [T.Text] -> (T.Text, Word32)
getChecksumPair s d =
  let joinedSrc = T.concat $ L.intersperse ":" s
      joinedDest = T.concat $ L.intersperse ":" d
  in (joinedDest, Hash.crc32 (TE.encodeUtf8 joinedSrc))
  
buildFoldFunc :: Either [SrcTransform] T.Text -> Target -> BuildM (Either [SrcTransform] T.Text)
buildFoldFunc (Left _) t@(Target name _ _ _ _) = do
  buildState <- get
  let oldTargetName = getCurrentTargetName buildState
  put $ putCurrentTargetName buildState name
  result <- runTarget t
  writePendingDBUpdates
  newBuildState <- get
  put $ putCurrentTargetName newBuildState oldTargetName
  return result

buildFoldFunc r@(Right _) _ = return r

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

runTarget :: Target -> BuildM (Either [SrcTransform] T.Text)
runTarget t@(Target name _ deps _ _) = do
  buildState <- get
  let outdatedTargets = filter (not.targetIsUpToDate buildState) deps
  depStatus <- foldM buildFoldFunc (Left []) outdatedTargets
  if isLeft depStatus then runTargetInternal t else buildFailFunc depStatus name
  
buildFailFunc :: Either [SrcTransform] T.Text -> T.Text -> BuildM (Either [SrcTransform] T.Text)
buildFailFunc (Right err) name = do
  liftIO printSeparator
  liftIO $ putStr $ "ERROR: Error building target \"" ++ T.unpack name ++ "\": "
  liftIO $ putStrLn $ T.unpack err
  return $ Right ""
buildFailFunc (Left _) _ = return $ Right ""

runTargetInternal :: Target -> BuildM (Either [SrcTransform] T.Text)
runTargetInternal t@(Target name hashFunc _ stages gatherers) = do
  buildState <- get
  let tcdb = getTargetChecksumDB buildState
  let checksum = hashFunc t
  let forceRebuild = checksum /= (Map.findWithDefault 0 name tcdb)
  gatheredFiles <- liftIO $ runGatherers gatherers
  let srcTransforms = map (flip OneToOne "") gatheredFiles
  liftIO $ putStrLn $ "==== Target: \"" ++ T.unpack name ++ "\""
  stageResult <- foldM stageFoldFunc (Left srcTransforms) $ zip stages $ repeat forceRebuild
  if isLeft stageResult then targetSuccessFunc t else buildFailFunc stageResult name

targetSuccessFunc :: Target -> BuildM (Either [SrcTransform] T.Text)
targetSuccessFunc t@(Target name hashFunc _ _ _) = do
  buildState <- get
  let updatedTargets = Set.insert t $ getUpToDateTargets buildState
  let updatedChecksums = Map.insert name (hashFunc t) $ getTargetChecksumDB buildState
  put $ putTargetChecksumDB (putUpToDateTargets buildState updatedTargets) updatedChecksums 
  liftIO $ putStrLn $ "Successfully built target \"" ++ T.unpack name ++ "\""
  liftIO $ putStrLn ""
  return $ Left []

stageFoldFunc :: Either [SrcTransform] T.Text -> (Stage, Bool) -> BuildM (Either [SrcTransform] T.Text)
stageFoldFunc (Left t) (s, force) = runStage s force t
stageFoldFunc r@(Right _) _ = return r

workerThreadFunc :: (SrcTransform -> IO (Either SrcTransform T.Text)) -> MVar [SrcTransform] -> MVar (Either [SrcTransform] T.Text, [BuildM ()]) -> MVar (Either [SrcTransform] T.Text, [BuildM ()]) -> MVar Int -> IO ()
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
      let combine right@(Right _) _ = right
          combine (Left ml) (Left v) = Left (v : ml)
          combine (Left _) (Right v) = Right v
      let newResultAcc = (\(res, thunks) -> (combine res taskResult, dbThunk : thunks)) resultAcc
      putMVar r newResultAcc
      workerThreadFunc sf q r f c

stageHelper :: (SrcTransform -> IO (Either SrcTransform T.Text)) -> Int -> [SrcTransform] -> Either [SrcTransform] T.Text -> BuildM (Either [SrcTransform] T.Text)
stageHelper f m i r = do
  finalResultMVar <- liftIO newEmptyMVar
  resultMVar <- liftIO $ newMVar (r, []) -- (overall result, database thunks)
  queueMVar <- liftIO $ newMVar i
  threadCountMVar <- liftIO $ newMVar m
  if null i then do
      return r
    else do
      liftIO $ mapM_ forkIO (take m $ repeat $ workerThreadFunc f queueMVar resultMVar finalResultMVar threadCountMVar)
      result <- liftIO $ takeMVar finalResultMVar
      sequence_ $ snd result
      return $ fst result

runStage :: Stage -> Bool -> [SrcTransform] -> BuildM (Either [SrcTransform] T.Text)
runStage s@(Stage name _ _ extraDeps f) force m = do
  liftIO $ putStrLn $ "-- Stage: \"" ++ T.unpack name ++ "\""
  depScannedFiles <- liftIO $ processMappings s m
  (targetsToBuild, upToDateTargets) <- partitionMappings depScannedFiles extraDeps force
  bs <- get
  result <- stageHelper f (getMaxBuildJobs bs) targetsToBuild (Left $ map transferUpToDateTarget upToDateTargets)
  updateDatabaseExtraDeps result extraDeps

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
updateDatabase (Right _) _ = return ()
updateDatabase (Left _) (OneToOne s d) = updateDatabaseHelper [s] [d]
updateDatabase (Left _) (OneToMany s ds) = updateDatabaseHelper [s] ds
updateDatabase (Left _) (ManyToOne ss d) = updateDatabaseHelper ss [d]
updateDatabase (Left _) (ManyToMany ss ds) = updateDatabaseHelper ss ds

updateDatabaseHelper :: [T.Text] -> [T.Text] -> BuildM ()
updateDatabaseHelper srcFiles destFiles = do
  buildstate <- get
  let pdbu = getPendingDBUpdates buildstate
  timestamps <- liftIO $ mapM getTimestamp srcFiles
  let filteredResults = filter (\(_, v) -> v /= 0) $ zip srcFiles timestamps
  let updatedPDBU = L.foldl' (\m (k, v) -> Map.insert k v m) pdbu filteredResults 
  let cdb = getChecksumDB buildstate
  let (key, cs) = getChecksumPair srcFiles destFiles
  let updatedCDB = Map.insert key cs cdb
  put $ putChecksumDB (putPendingDBUpdates buildstate updatedPDBU) updatedCDB
  return ()

updateDatabaseExtraDeps :: (Either [SrcTransform] T.Text) -> [T.Text] -> BuildM (Either [SrcTransform] T.Text)
updateDatabaseExtraDeps result@(Right _) _ = return result
updateDatabaseExtraDeps result@(Left _) deps = do
  buildstate <- get
  let pdbu = getPendingDBUpdates buildstate
  timestamps <- liftIO $ mapM getTimestamp deps
  let filteredResults = filter (\(_, v) -> v /= 0) $ zip deps timestamps
  let updatedPDBU = L.foldl' (\m (k, v) -> Map.insert k v m) pdbu filteredResults 
  put $ putPendingDBUpdates buildstate updatedPDBU
  return result

writePendingDBUpdates :: BuildM ()
writePendingDBUpdates = do
  buildstate <- get
  let tdb = getTimestampDB buildstate
  let pdbu = getPendingDBUpdates buildstate
  let updatedTDB = Map.union pdbu tdb
  put $ putPendingDBUpdates (putTimestampDB buildstate updatedTDB) Map.empty
  return ()
