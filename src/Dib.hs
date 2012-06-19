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
import Control.Monad.State as S
import qualified Data.ByteString as B
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Serialize as Serialize
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified System.Directory as D
import qualified System.Time as Time
import Data.Maybe

dib :: [Target] -> T.Text -> IO ()
dib targets selectedTarget = do
  let theTarget = L.find (\(Target name _ _ _) -> name == selectedTarget) targets
  if isNothing theTarget
    then putStrLn $ "ERROR: Invalid target specified: \"" ++ T.unpack selectedTarget ++ "\"" else
    do
      db <- loadTimestampDB 
      (_, s) <- runBuild (runTarget $ fromJust theTarget) (BuildState [] db Set.empty)
      saveTimestampDB $ getTimestampDB s
      return ()

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

-- | Returns whether or not a target is up to date, based on the current build state. 
targetIsUpToDate :: BuildState -> Target -> Bool
targetIsUpToDate (BuildState _ _ s) t = Set.member t s

-- | Filters out up-to-date mappings
filterMappings :: [SrcTransform] -> BuildM [SrcTransform]
filterMappings files = get >>= \(BuildState _ db _) -> liftIO $ filterM (shouldBuildMapping db) files

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
  liftIO $ putStrLn $ "ERROR: Error building target \"" ++ T.unpack name ++ "\":"
  liftIO $ putStrLn $ T.unpack err
  return $ Right ""
buildFailFunc (Left _) _ = return $ Right ""

runTargetInternal :: Target -> BuildM (Either [SrcTransform] T.Text)
runTargetInternal t@(Target name _ stages gatherers) = do
  gatheredFiles <- liftIO $ runGatherers gatherers
  let srcTransforms = map (flip OneToOne "") gatheredFiles
  targetUpToDate <- needToRunStage (head stages) srcTransforms
  if targetUpToDate
    then (liftIO $ putStrLn $ "Target is up to date: \"" ++ T.unpack name ++ "\"") >> return (Left []) else
    do
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

needToRunStage :: Stage -> [SrcTransform] -> BuildM Bool
needToRunStage s m = do
  depScannedFiles <- liftIO $ processMappings s m
  filteredMappings <- filterMappings depScannedFiles
  return $ not $ null filteredMappings

runStage :: Stage -> [SrcTransform] -> BuildM (Either [SrcTransform] T.Text)
runStage s@(Stage _ _ _ f) m = do 
  depScannedFiles <- liftIO $ processMappings s m
  filteredMappings <- filterMappings depScannedFiles
  liftIO $ foldM foldFunc (Left []) filteredMappings
  where foldFunc r@(Right _) _ = return r
        foldFunc a@(Left _) b = do
          result <- f b
          return $ combine a result
          where combine r@(Right _) _ = r
                combine (Left ml) (Left v) = Left (ml ++ [v])
                combine (Left _) (Right v) = Right v
  
processMappings :: Stage -> [SrcTransform] -> IO [SrcTransform]
processMappings (Stage _ t d _) m = do
  let transMap = map t m --transform input-only mappings into input -> output mappings
  mapM d transMap
