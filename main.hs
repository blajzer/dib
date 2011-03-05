module Main where

import GHC.IO.Exception
import System.Cmd (system)
import qualified System.Directory as D

findDib lastPath = do
    curPath <- D.getCurrentDirectory
    if curPath /= lastPath then
        D.doesFileExist "dib.hs">>= \hasDib -> if hasDib then system "runhaskell dib.hs" else (D.setCurrentDirectory "../" >> findDib curPath)
     else
        putStrLn "Error: can't find dib.hs." >> return (ExitFailure 255)

main = do
    currentDir <- D.getCurrentDirectory
    exitcode <- findDib ""
    D.setCurrentDirectory currentDir
    return exitcode
