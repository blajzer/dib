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
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Serialize as Serialize
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified System.Directory as D
import qualified System.Environment as Env
import qualified System.Time as Time
import Data.Maybe

dib :: [Target] -> IO ()
dib targets = do
  args <- Env.getArgs
  let buildArgs = parseArgs args targets
  let selectedTarget = (buildTarget buildArgs)
  let theTarget = L.find (\(Target name _ _ _) -> name == selectedTarget) targets
  if isNothing theTarget
    then putStrLn $ "ERROR: Invalid target specified: \"" ++ T.unpack selectedTarget ++ "\"" else
    do
      db <- loadTimestampDB 
      (_, s) <- runBuild (runTarget $ fromJust theTarget) (BuildState buildArgs db Set.empty)
      saveTimestampDB $ getTimestampDB s
      return ()

parseArgs :: [String] -> [Target] -> BuildArgs
parseArgs args targets =
  let argsLen = length args
      target = if argsLen > 0 then (T.pack.head $ args) else (T.pack.show.head $ targets)
  in BuildArgs { buildTarget = target, maxBuildJobs = 2 }

printSeparator :: IO ()
printSeparator = putStrLn "============================================================"

runBuild :: BuildM a -> BuildState -> IO (a, BuildState)
runBuild m = runStateT (runBuildImpl m)

loadTimestampDB :: IO TimestampDB
loadTimestampDB = do fileExists <- D.doesFileExist ".dibdb"
                     fileContents <- if fileExists then B.readFile ".dibdb" else return B.empty
                     return.handleEither $ Serialize.decode fileContents
                     where handleEither (Left _) = Map.empty
                           handleEither (Right a) = a

saveTimestampDB :: TimestampDB -> IO ()
saveTimestampDB m = B.writeFile ".dibdb" $ Serialize.encode m

getTimestampDB :: BuildState -> TimestampDB
getTimestampDB (BuildState _ db _) = db

putTimestampDB :: BuildState -> TimestampDB -> BuildState
putTimestampDB (BuildState a _ t) db = BuildState a db t

getUpToDateTargets :: BuildState -> UpToDateTargets
getUpToDateTargets (BuildState _ _ t) = t

putUpToDateTargets :: BuildState -> UpToDateTargets -> BuildState
putUpToDateTargets (BuildState a db _) = BuildState a db

getMaxBuildJobs :: BuildState -> Int
getMaxBuildJobs (BuildState a _ _) = maxBuildJobs a

-- | Returns whether or not a target is up to date, based on the current build state. 
targetIsUpToDate :: BuildState -> Target -> Bool
targetIsUpToDate (BuildState _ _ s) t = Set.member t s

-- | Filters out up-to-date mappings
filterMappings :: [SrcTransform] -> BuildM [SrcTransform]
filterMappings files = get >>= \(BuildState _ db _) -> liftIO $ filterM (shouldBuildMapping db) files

-- | Partitions out up-to-date mappings
partitionMappings :: [SrcTransform] -> BuildM ([SrcTransform], [SrcTransform])
partitionMappings files = do
  s <- get
  shouldBuild <- liftIO $ mapM (shouldBuildMapping (getTimestampDB s)) files
  let paired = zip shouldBuild files
  let (a, b) = L.partition fst paired
  return $ ((map snd a), (map snd b))
  

(<||>) :: IO Bool -> IO Bool -> IO Bool
(<||>) = liftM2 (||)

-- function for filtering FileMappings based on them already being taken care of
shouldBuildMapping :: TimestampDB -> SrcTransform -> IO Bool
shouldBuildMapping m (OneToOne s d) = hasSrcChanged m [s] <||> liftM not (D.doesFileExist $ T.unpack d)
shouldBuildMapping m (OneToMany s ds) = hasSrcChanged m [s] <||> liftM (not.and) (mapM (D.doesFileExist.T.unpack) ds)
shouldBuildMapping m (ManyToOne ss d) = hasSrcChanged m ss <||> liftM not (D.doesFileExist $ T.unpack d)
shouldBuildMapping m (ManyToMany ss ds) = hasSrcChanged m ss <||> liftM (not.and) (mapM (D.doesFileExist.T.unpack) ds)

hasSrcChanged :: TimestampDB -> [T.Text] -> IO Bool
hasSrcChanged m f = let filesInMap = zip f $ map (flip Map.lookup m) f
                        checkTimeStamps _ (_, Nothing) = return True
                        checkTimeStamps b (file, Just s) = getTimestamp file >>= (\t -> return $ b || (t /= s))
                    in foldM checkTimeStamps False filesInMap

getTimestamp :: T.Text -> IO Integer
getTimestamp f = do
  let unpackedFileName = T.unpack f 
  doesExist <- D.doesFileExist unpackedFileName
  if doesExist then D.getModificationTime unpackedFileName >>= extractSeconds else return 0
  where extractSeconds (Time.TOD s _) = return s
  
buildFoldFunc :: Either [SrcTransform] T.Text -> Target -> BuildM (Either [SrcTransform] T.Text)
buildFoldFunc (Left _) t = runTarget t
buildFoldFunc r@(Right _) _ = return r

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

runTarget :: Target -> BuildM (Either [SrcTransform] T.Text)
runTarget t@(Target name deps _ _) = do
  buildState <- get
  let outdatedTargets = filter (\x -> not $ targetIsUpToDate buildState x) deps
  depStatus <- foldM buildFoldFunc (Left []) outdatedTargets
  if isLeft depStatus then runTargetInternal t else buildFailFunc depStatus name
  
buildFailFunc :: Either [SrcTransform] T.Text -> T.Text -> BuildM (Either [SrcTransform] T.Text)
buildFailFunc (Right err) name = do
  liftIO $ printSeparator
  liftIO $ putStr $ "ERROR: Error building target \"" ++ T.unpack name ++ "\": "
  liftIO $ putStrLn $ T.unpack err
  return $ Right ""
buildFailFunc (Left _) _ = return $ Right ""

runTargetInternal :: Target -> BuildM (Either [SrcTransform] T.Text)
runTargetInternal t@(Target name _ stages gatherers) = do
  gatheredFiles <- liftIO $ runGatherers gatherers
  let srcTransforms = map (flip OneToOne "") gatheredFiles
  liftIO $ putStrLn $ "Building target \"" ++ T.unpack name ++ "\""
  liftIO $ printSeparator
  stageResult <- foldM stageFoldFunc (Left srcTransforms) stages
  if isLeft stageResult then targetSuccessFunc t else buildFailFunc stageResult name

targetSuccessFunc :: Target -> BuildM (Either [SrcTransform] T.Text)
targetSuccessFunc t@(Target name _ _ _) = do
  buildState <- get
  put $ putUpToDateTargets buildState $ Set.insert t $ getUpToDateTargets buildState
  liftIO $ putStrLn $ "Successfully built target \"" ++ T.unpack name ++ "\""
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
  results <- liftIO $ mapM tryTakeMVar (map snd a)
  let r = map fromJust $ L.filter isJust results
  let (active, done) = L.partition (isNothing.fst) (zip results a)
  let numActive = L.length active
  let (toSpawn, theRest) = L.splitAt (m - numActive) i
  futures <- liftIO $ mapM future (map f toSpawn)
  mapM_ (uncurry writeTimestamps) (map (\((Just res), (src, _)) -> (res, src)) done)
  return (theRest, (zip toSpawn futures) ++ (map snd active), r)

-- recursively spawns new tasks
stageHelper :: (SrcTransform -> IO (Either a t)) -> Int -> [SrcTransform] -> [(SrcTransform, MVar (Either a t))] -> Either [a] t -> BuildM (Either [a] t)
stageHelper f m i a r = do
  let combine right@(Right _) _ = right
      combine (Left ml) (Left v) = Left (ml ++ [v])
      combine (Left _) (Right v) = Right v
  if (L.length i) > 0  || (L.length a) > 0 then
    do
      (i', a', r') <- spawnStageThreads f m i a
      stageHelper f m i' a' (L.foldl' combine r r')
   else
    do return r

runStage :: Stage -> [SrcTransform] -> BuildM (Either [SrcTransform] T.Text)
runStage s@(Stage name _ _ f) m = do
  liftIO $ putStrLn $ "--------------- Running stage \"" ++ (T.unpack name) ++ "\" ---------------"
  depScannedFiles <- liftIO $ processMappings s m
  (targetsToBuild, upToDateTargets) <- partitionMappings depScannedFiles
  bs <- get
  results <- stageHelper f (getMaxBuildJobs bs) targetsToBuild [] (Left $ map transferUpToDateTarget upToDateTargets)
  return results

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

writeTimestamps :: Either l r -> SrcTransform -> BuildM ()
writeTimestamps (Right _) _ = return ()
writeTimestamps (Left _) (OneToOne s _) = writeTimestampHelper $ [s]
writeTimestamps (Left _) (OneToMany s _) = writeTimestampHelper $ [s]
writeTimestamps (Left _) (ManyToOne ss _) = writeTimestampHelper $ ss
writeTimestamps (Left _) (ManyToMany ss _) = writeTimestampHelper $ ss

writeTimestampHelper :: [T.Text] -> BuildM ()
writeTimestampHelper files = do
  buildstate <- get
  let db = getTimestampDB buildstate
  timestamps <- liftIO $ mapM getTimestamp files
  let filteredResults = filter (\(_,y) -> y /= 0) $ zip files timestamps
  let updatedDB = L.foldl' (\d (f, t) -> Map.insert f t d) db filteredResults
  put $ putTimestampDB buildstate updatedDB
  return ()
