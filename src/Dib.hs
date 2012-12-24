module Dib (
  SrcTransform(OneToOne, OneToMany, ManyToOne, ManyToMany),
  BuildState,
  dib,
  filterMappings,
  getTimestampDB,
  putTimestampDB,
  targetIsUpToDate
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
import qualified System.Directory as D
import qualified System.Environment as Env
import qualified System.Time as Time
import Data.Maybe
import Data.Word

databaseFile :: String
databaseFile = ".dib/dibdb"

databaseVersion :: Integer
databaseVersion = 1

dib :: [Target] -> IO ()
dib targets = do
  args <- Env.getArgs
  let buildArgs = parseArgs args targets
  let selectedTarget = buildTarget buildArgs
  let theTarget = L.find (\(Target name _ _ _) -> name == selectedTarget) targets
  if isNothing theTarget
    then putStrLn $ "ERROR: Invalid target specified: \"" ++ T.unpack selectedTarget ++ "\"" else
    do
      (tdb, cdb) <- loadDatabase
      (_, s) <- runBuild ((runTarget $ fromJust theTarget) >> writePendingDBUpdates) (BuildState buildArgs tdb cdb Set.empty [])
      saveDatabase (getTimestampDB s) (getChecksumDB s)
      return ()

parseArgs :: [String] -> [Target] -> BuildArgs
parseArgs args targets =
  let argsLen = length args
      target = if argsLen > 0 then T.pack.head $ args else T.pack.show.head $ targets
  in BuildArgs { buildTarget = target, maxBuildJobs = 2 }

printSeparator :: IO ()
printSeparator = putStrLn "============================================================"

runBuild :: BuildM a -> BuildState -> IO (a, BuildState)
runBuild m = runStateT (runBuildImpl m)

loadDatabase :: IO (TimestampDB, ChecksumDB)
loadDatabase = do fileExists <- D.doesFileExist databaseFile
                  fileContents <- if fileExists then B.readFile databaseFile else return B.empty
                  return.handleEither $ Serialize.decode fileContents
                  where handleEither (Left _) = (Map.empty, Map.empty)
                        handleEither (Right (v, t, c)) = if v == databaseVersion then (t, c) else (Map.empty, Map.empty)

saveDatabase :: TimestampDB -> ChecksumDB -> IO ()
saveDatabase tdb cdb = B.writeFile databaseFile $ Serialize.encode (databaseVersion, tdb, cdb)

getTimestampDB :: BuildState -> TimestampDB
getTimestampDB (BuildState _ tdb _ _ _) = tdb

putTimestampDB :: BuildState -> TimestampDB -> BuildState
putTimestampDB (BuildState a _ c t p) tdb = BuildState a tdb c t p

getChecksumDB :: BuildState -> ChecksumDB
getChecksumDB (BuildState _ _ cdb _ _) = cdb

putChecksumDB :: BuildState -> ChecksumDB -> BuildState
putChecksumDB (BuildState a tdb _ t p) cdb = BuildState a tdb cdb t p

getUpToDateTargets :: BuildState -> UpToDateTargets
getUpToDateTargets (BuildState _ _ _ t _) = t

putUpToDateTargets :: BuildState -> UpToDateTargets -> BuildState
putUpToDateTargets (BuildState a tdb cdb _ p) t = BuildState a tdb cdb t p

getPendingDBUpdates :: BuildState -> PendingDBUpdates
getPendingDBUpdates (BuildState _ _ _ _ p) = p

putPendingDBUpdates :: BuildState -> PendingDBUpdates -> BuildState
putPendingDBUpdates (BuildState a tdb cdb t _) p = BuildState a tdb cdb t p

getMaxBuildJobs :: BuildState -> Int
getMaxBuildJobs (BuildState a _ _ _ _) = maxBuildJobs a

-- | Returns whether or not a target is up to date, based on the current build state. 
targetIsUpToDate :: BuildState -> Target -> Bool
targetIsUpToDate (BuildState _ _ _ s _) t = Set.member t s

-- | Filters out up-to-date mappings
filterMappings :: [SrcTransform] -> BuildM [SrcTransform]
filterMappings files = get >>= \(BuildState _ tdb cdb _ _) -> liftIO $ filterM (shouldBuildMapping tdb cdb) files

-- | Partitions out up-to-date mappings
partitionMappings :: [SrcTransform] -> BuildM ([SrcTransform], [SrcTransform])
partitionMappings files = do
  s <- get
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
  where extractSeconds (Time.TOD s _) = return s

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
buildFoldFunc (Left _) t = runTarget t
buildFoldFunc r@(Right _) _ = return r

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

runTarget :: Target -> BuildM (Either [SrcTransform] T.Text)
runTarget t@(Target name deps _ _) = do
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
runTargetInternal t@(Target name _ stages gatherers) = do
  gatheredFiles <- liftIO $ runGatherers gatherers
  let srcTransforms = map (flip OneToOne "") gatheredFiles
  liftIO $ putStrLn $ "Building target \"" ++ T.unpack name ++ "\""
  liftIO printSeparator
  stageResult <- foldM stageFoldFunc (Left srcTransforms) stages
  if isLeft stageResult then targetSuccessFunc t else buildFailFunc stageResult name

targetSuccessFunc :: Target -> BuildM (Either [SrcTransform] T.Text)
targetSuccessFunc t@(Target name _ _ _) = do
  buildState <- get
  put $ putUpToDateTargets buildState $ Set.insert t $ getUpToDateTargets buildState
  liftIO $ putStrLn $ "Successfully built target \"" ++ T.unpack name ++ "\""
  liftIO $ putStrLn ""
  return $ Left []

stageFoldFunc :: Either [SrcTransform] T.Text -> Stage -> BuildM (Either [SrcTransform] T.Text)
stageFoldFunc (Left t) s = runStage s t
stageFoldFunc r@(Right _) _ = return r

future :: IO a -> IO (MVar a)
future thunk = do
    ref <- newEmptyMVar
    _ <- forkIO $ thunk >>= putMVar ref
    return ref

-- returns tuple of rest of input, active threads, and results
spawnStageThreads :: (SrcTransform -> IO (Either l r)) -> Int -> [SrcTransform] -> [(SrcTransform, MVar (Either l r))] -> BuildM ([SrcTransform], [(SrcTransform, MVar (Either l r))], [Either l r])
spawnStageThreads f m i a = do
  results <- liftIO $ mapM (tryTakeMVar.snd) a
  let r = map fromJust $ L.filter isJust results
  let (active, done) = L.partition (isNothing.fst) (zip results a)
  let numActive = L.length active
  let (toSpawn, theRest) = L.splitAt (m - numActive) i
  futures <- liftIO $ mapM (future.f) toSpawn
  mapM_ (uncurry updateDatabase.(\(Just res, (src, _)) -> (res, src))) done
  return (theRest, zip toSpawn futures ++ map snd active, r)

-- recursively spawns new tasks
stageHelper :: (SrcTransform -> IO (Either a t)) -> Int -> [SrcTransform] -> [(SrcTransform, MVar (Either a t))] -> Either [a] t -> BuildM (Either [a] t)
stageHelper f m i a r = do
  let combine right@(Right _) _ = right
      combine (Left ml) (Left v) = Left (ml ++ [v])
      combine (Left _) (Right v) = Right v
  if L.length i > 0  || L.length a > 0 then do
      (i', a', r') <- spawnStageThreads f m i a
      stageHelper f m i' a' (L.foldl' combine r r')
   else
      return r

runStage :: Stage -> [SrcTransform] -> BuildM (Either [SrcTransform] T.Text)
runStage s@(Stage name _ _ f) m = do
  liftIO $ putStrLn $ "--------------- Running stage \"" ++ T.unpack name ++ "\" ---------------"
  depScannedFiles <- liftIO $ processMappings s m
  (targetsToBuild, upToDateTargets) <- partitionMappings depScannedFiles
  bs <- get
  stageHelper f (getMaxBuildJobs bs) targetsToBuild [] (Left $ map transferUpToDateTarget upToDateTargets)

-- These might not be quite correct. I guessed at what made sense.
transferUpToDateTarget :: SrcTransform -> SrcTransform
transferUpToDateTarget (OneToOne _ d) = OneToOne d ""
transferUpToDateTarget (OneToMany _ ds) = ManyToOne ds ""
transferUpToDateTarget (ManyToOne _ d) = OneToOne d ""
transferUpToDateTarget (ManyToMany _ ds) = ManyToOne ds ""

processMappings :: Stage -> [SrcTransform] -> IO [SrcTransform]
processMappings (Stage _ t d _) m = do
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
  let filteredResults = filter (\(_,y) -> y /= 0) $ zip srcFiles timestamps
  let updatedPDBU = pdbu ++ filteredResults
  let cdb = getChecksumDB buildstate
  let (key, cs) = getChecksumPair srcFiles destFiles
  let updatedCDB = Map.insert key cs cdb
  put $ putChecksumDB (putPendingDBUpdates buildstate updatedPDBU) updatedCDB
  return ()

writePendingDBUpdates :: BuildM ()
writePendingDBUpdates = do
  buildstate <- get
  let tdb = getTimestampDB buildstate
  let pdbu = getPendingDBUpdates buildstate
  let updatedTDB = L.foldl' (\d (f, t) -> Map.insert f t d) tdb pdbu
  put $ putPendingDBUpdates (putTimestampDB buildstate updatedTDB) []
  return ()
