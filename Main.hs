module Main where

import Data.List (intercalate)
import GHC.IO.Exception
import System.Cmd (system)
import qualified System.Directory as D
import System.Environment (getArgs)
import System.IO
import System.Time as T

unixExe = ".dib/dib"
windowsExe = ".dib/dib.exe"
timestampFile = ".dib/timestamp"

buildString = "ghc -o .dib/dib -O2 -outputdir .dib dib.hs"

findDib :: FilePath -> [Char] -> IO ExitCode
findDib lastPath args = do
    curPath <- D.getCurrentDirectory
    if curPath /= lastPath then
        D.doesFileExist "dib.hs" >>= \hasDib -> if hasDib then buildAndRunDib args else (D.setCurrentDirectory "../" >> findDib curPath args)
     else
        putStrLn "Error: can't find dib.hs." >> return (ExitFailure 255)

ensureDibDirExists :: IO ()
ensureDibDirExists = do
  dibDirExists <- D.doesDirectoryExist ".dib"
  if dibDirExists then
      return ()
    else
      D.createDirectory ".dib"

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

rebuild :: Bool -> IO ()
rebuild needToRebuild = do
  if needToRebuild then do
      _ <- system buildString
      calTimeStr <- getDibCalendarTimeStr
      tsFile <- openFile timestampFile WriteMode
      hPutStr tsFile calTimeStr
      hClose tsFile
    else
      return ()

buildAndRunDib :: [Char] -> IO ExitCode
buildAndRunDib args = do
  ensureDibDirExists
  needToRebuild <- checkDibTimestamps
  rebuild needToRebuild
  system $ ".dib/dib " ++ args

main :: IO ExitCode
main = do
    args <- getArgs
    currentDir <- D.getCurrentDirectory
    exitcode <- findDib "" (intercalate "" args)
    D.setCurrentDirectory currentDir
    return exitcode
