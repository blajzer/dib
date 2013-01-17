module Main where

import Data.List (intercalate)
import GHC.IO.Exception
import Control.Monad
import System.Cmd (system)
import qualified System.Directory as D
import System.Environment (getArgs)
import System.IO
import System.Time as T

unixExe :: String
unixExe = ".dib/dib"

windowsExe :: String
windowsExe = ".dib/dib.exe"

timestampFile :: String
timestampFile = ".dib/timestamp"

buildString :: String
buildString = "ghc -o .dib/dib -O2 -XOverloadedStrings -rtsopts -threaded -outputdir .dib dib.hs"

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
   do exitCode <- system buildString
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
  system $ ".dib/dib +RTS -N -RTS " ++ args

main :: IO ExitCode
main = do
    args <- getArgs
    currentDir <- D.getCurrentDirectory
    exitcode <- findDib "" (intercalate "" $ map requoteArg args)
    D.setCurrentDirectory currentDir
    return exitcode
