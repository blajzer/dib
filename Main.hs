-- | This is the command-line executable "dib". Since Dib proper is a
-- library, one doesn't need this, but it makes dealing with build
-- scripts quite a bit easier.
module Main where

import Data.List (intercalate)
import GHC.IO.Exception
import Control.Monad
import System.Cmd (system)
import qualified System.Directory as D
import System.Environment (getArgs)
import System.Info
import System.IO
import System.Time as T

unixExe :: String
unixExe = ".dib/dib"

windowsExe :: String
windowsExe = ".dib/dib.exe"

correctExe :: String
correctExe = if os == "mingw32" then windowsExe else unixExe

-- | Location to copy the dib.hs script to and rename it. Get around
-- an issue with building on Windows.
tmpDibScript :: String
tmpDibScript = ".dib/dib-tmp.hs"

-- | The file that stores the timestamp for dib.hs
timestampFile :: String
timestampFile = ".dib/timestamp"

-- | The command line for building dib.hs
buildString :: String
buildString = "ghc -o " ++ correctExe ++ " -O2 -XOverloadedStrings -rtsopts -threaded -outputdir .dib " ++ tmpDibScript

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
compilerToConfig _ = undefined

-- | Makes a C Builder. Takes a name and source directory.
cBuilderScript :: String -> String -> String -> String
cBuilderScript name compiler srcDir = "\
  \module Main where\n\n\
  \import Dib\n\
  \import Dib.Builders.C\n\
  \import qualified Data.Text as T\n\n"
  ++ "projectInfo = " ++ (compilerToConfig compiler) ++ " {\n"
  ++ "  projectName = \"" ++ name ++ "\",\n"
  ++ "  srcDir = \"" ++ srcDir ++ "\",\n"
  ++ "  compileFlags = \"\",\n"
  ++ "  linkFlags = \"\",\n"
  ++ "  includeDirs = [\"" ++ srcDir ++ "\"]\n"
  ++ "}\n\n"
  ++ "project = makeCTarget projectInfo\n\
  \clean = makeCleanTarget projectInfo\n\n\
  \targets = [project, clean]\n\n\
  \main = dib targets"

-- | Recurses up the file path until it finds a directory with a dib.hs in it
-- Once it finds a dib.hs, it builds and runs it.
findDib :: FilePath -> String -> IO ExitCode
findDib lastPath args = do
    curPath <- D.getCurrentDirectory
    if curPath /= lastPath then
        D.doesFileExist "dib.hs" >>= \hasDib -> if hasDib then buildAndRunDib args else D.setCurrentDirectory "../" >> findDib curPath args
     else
        putStrLn "Error: can't find dib.hs." >> return (ExitFailure 255)

getDibCalendarTimeStr :: IO String
getDibCalendarTimeStr = do
  modTime <- D.getModificationTime "dib.hs"
  calTime <- T.toCalendarTime modTime
  return $ T.calendarTimeToString calTime

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
   do D.copyFile "dib.hs" tmpDibScript
      exitCode <- system buildString
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
shouldHandleInit args = (length args) >= 1 && (head args) == "--init"	

argsToBuildScript :: [String] -> String
argsToBuildScript ["c", name, compiler, srcDir] = cBuilderScript name compiler srcDir
argsToBuildScript ["empty"] = defaultScript
argsToBuildScript [] = defaultScript
argsToBuildScript _ = error "Error: Unknown template name."

handleInit :: [String] -> IO Bool
handleInit args = do
  if shouldHandleInit args then do
      scriptExists <- D.doesFileExist "dib.hs"
      if not scriptExists then do
          let buildScript = argsToBuildScript $ tail args
          putStrLn "Initializing dib.hs..."
          withFile "dib.hs" WriteMode (\h -> hPutStr h buildScript >> return True) 
        else do
          error "Error: dib.hs already exists"
    else
      return False

main :: IO ExitCode
main = do
    args <- getArgs
    inited <- handleInit args
    if inited then
        return ExitSuccess 
      else do
        currentDir <- D.getCurrentDirectory
        exitcode <- findDib "" (intercalate "" $ map requoteArg args)
        D.setCurrentDirectory currentDir
        return exitcode
