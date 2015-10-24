-- | This is the command-line executable "dib". Since Dib proper is a
-- library, one doesn't need this, but it makes dealing with build
-- scripts quite a bit easier.
module Main where

import GHC.IO.Exception
import Control.Monad
import Data.Maybe
import Data.Time.Clock.POSIX
import System.Process (system)
import qualified System.Directory as D
import System.Environment (getArgs)
import System.FilePath as F
import System.Info
import System.IO

unixExe :: String
unixExe = ".dib/dib"

windowsExe :: String
windowsExe = ".dib/dib.exe"

correctExe :: String
correctExe = if os == "mingw32" then windowsExe else unixExe

correctExtension :: String
correctExtension = if os == "mingw32" then ".exe" else ""

-- | Location to copy the dib.hs script to and rename it. Get around
-- an issue with building on Windows.
tmpDibScript :: String
tmpDibScript = "dib-tmp.hs"

-- | The file that stores the timestamp for dib.hs
timestampFile :: String
timestampFile = ".dib/timestamp"

-- | The command line for building dib.hs
buildString :: String
buildString = "ghc -o dib" ++ correctExtension ++ " -O2 -XOverloadedStrings -rtsopts -threaded -outputdir . " ++ tmpDibScript

-- | A basic dib script. This is the script saved when running "dib init"
defaultScript :: String
defaultScript = "\
  \module Main where\n\n\
  \import Dib\n\
  \import qualified Data.Text as T\n\n\
  \targets = []\n\n\
  \main = dib targets\n"

compilerToConfig :: String -> String
compilerToConfig "gcc" = "defaultGCCConfig"
compilerToConfig "g++" = "defaultGXXConfig"
compilerToConfig "clang" = "defaultClangConfig"
compilerToConfig template = error $ "Error: unknown template \"" ++ template ++ "\""

-- | Makes a C Builder. Takes a name and source directory.
cBuilderScript :: String -> String -> String -> String
cBuilderScript name compiler srcDir = "\
  \module Main where\n\n\
  \import Dib\n\
  \import Dib.Builders.C\n\
  \import qualified Data.Text as T\n\n"
  ++ "projectInfo = " ++ compilerToConfig compiler ++ " {\n"
  ++ "  outputName = \"" ++ name ++ "\",\n"
  ++ "  targetName = \"" ++ name ++ "\",\n"
  ++ "  srcDir = \"" ++ srcDir ++ "\",\n"
  ++ "  compileFlags = \"\",\n"
  ++ "  linkFlags = \"\",\n"
  ++ "  outputLocation = ObjAndBinDirs \"obj\" \".\",\n"
  ++ "  includeDirs = [\"" ++ srcDir ++ "\"]\n"
  ++ "}\n\n"
  ++ "project = makeCTarget projectInfo\n\
  \clean = makeCleanTarget projectInfo\n\n\
  \targets = [project, clean]\n\n\
  \main = dib targets"

-- | Recurses up the file path until it finds a directory with a dib.hs in it
-- Once it finds a dib.hs, it returns the path.
findDib :: FilePath -> IO (Maybe FilePath)
findDib lastPath = do
  let dibPath = lastPath </> "dib.hs"
  hasDib <- D.doesFileExist dibPath
  if hasDib then
      return $ Just dibPath
    else do
      curPath <- D.canonicalizePath $ lastPath </> ".."
      if curPath /= lastPath then
          findDib curPath
        else
          return Nothing

-- | Recurses up the file path until it finds a directory with a dib.hs in it
-- Once it finds a dib.hs, it builds and runs it.
findAndRunDib :: String -> IO ExitCode
findAndRunDib args = do
    curPath <- D.getCurrentDirectory
    dibPath <- findDib curPath
    if isJust dibPath then do
        D.setCurrentDirectory $ F.dropFileName $ fromJust dibPath
        retVal <- buildAndRunDib args
        D.setCurrentDirectory curPath
        return retVal
      else do
        putStrLn "Error: can't find dib.hs."
        putStrLn "Suggestion: use 'dib --init ...' to create one."
        putStrLn "  Empty Project: 'dib --init empty'"
        putStrLn "  C Project: 'dib --init c <projectName> (gcc|g++|clang) <srcDir>'"
        return (ExitFailure 255)

getDibCalendarTimeStr :: IO String
getDibCalendarTimeStr = do
  modTime <- D.getModificationTime "dib.hs"
  let calTime = (fromIntegral.fromEnum.utcTimeToPOSIXSeconds) modTime :: Integer
  return $ show calTime

checkDibTimestamps :: IO Bool
checkDibTimestamps = do
  dibUnixExists <- D.doesFileExist unixExe
  dibWinExists <- D.doesFileExist windowsExe
  if dibUnixExists || dibWinExists then do
      calTimeStr <- getDibCalendarTimeStr
      storedCalTime <- getStoredCalTime
      return $ calTimeStr /= storedCalTime
    else
      return True

getStoredCalTime :: IO String
getStoredCalTime = do
  timestampFileExists <- D.doesFileExist timestampFile
  if timestampFileExists then do
      tsFile <- openFile timestampFile ReadMode
      timestampStr <- hGetLine tsFile
      hClose tsFile
      return timestampStr
    else
      return ""

processExitCode :: ExitCode -> IO ()
processExitCode (ExitSuccess) = return ()
processExitCode (ExitFailure n) = error $ "Error " ++ show n ++ " building dib.hs."

rebuild :: Bool -> IO ()
rebuild needToRebuild =
  when needToRebuild $
   do D.copyFile "dib.hs" $ ".dib/" ++ tmpDibScript
      D.setCurrentDirectory ".dib"
      exitCode <- system buildString
      D.setCurrentDirectory ".."
      processExitCode exitCode
      calTimeStr <- getDibCalendarTimeStr
      tsFile <- openFile timestampFile WriteMode
      hPutStr tsFile calTimeStr
      hClose tsFile

requoteArg :: String -> String
requoteArg arg = requoteArgInternal arg False
  where
    requoteArgInternal ('=':xs) False = '=' : '\"' : requoteArgInternal xs True
    requoteArgInternal [] True = "\""
    requoteArgInternal [] False = []
    requoteArgInternal (x:xs) e = x : requoteArgInternal xs e

buildAndRunDib :: String -> IO ExitCode
buildAndRunDib args = do
  D.createDirectoryIfMissing False ".dib"
  needToRebuild <- checkDibTimestamps
  rebuild needToRebuild
  system $ correctExe ++ " +RTS -N -RTS " ++ args

shouldHandleInit :: [String] -> Bool
shouldHandleInit args = not (null args) && head args == "--init"

argsToBuildScript :: [String] -> String
argsToBuildScript ["c", name, compiler, srcDir] = cBuilderScript name compiler srcDir
argsToBuildScript ["empty"] = defaultScript
argsToBuildScript [] = defaultScript
argsToBuildScript _ = error "Error: Unknown template name."

handleInit :: [String] -> IO Bool
handleInit args =
  if shouldHandleInit args then do
      dibInCurDir <- D.doesFileExist "dib.hs"
      if dibInCurDir then
          error "Error: dib.hs already exists in current directory."
        else do
          curPath <- D.getCurrentDirectory
          dibPath <- findDib curPath
          let initPrefix = if isNothing dibPath then "" else "Warning: dib.hs exists in a parent directory, are you sure you want to do this?\n"
          let buildScript = argsToBuildScript $ tail args
          putStrLn $ initPrefix ++ "Initializing dib.hs..."
          withFile "dib.hs" WriteMode (\h -> hPutStr h buildScript >> return True)
    else
      return False

main :: IO ExitCode
main = do
    args <- getArgs
    inited <- handleInit args
    if inited then
        return ExitSuccess
      else
        findAndRunDib (unwords $ map requoteArg args)
