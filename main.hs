module Main where

import Data.List (intercalate)
import GHC.IO.Exception
import System.Cmd (system)
import qualified System.Directory as D
import System.Environment (getArgs)

findDib lastPath args = do
    curPath <- D.getCurrentDirectory
    if curPath /= lastPath then
        D.doesFileExist "dib.hs">>= \hasDib -> if hasDib then system ("runhaskell dib.hs " ++ args) else (D.setCurrentDirectory "../" >> findDib curPath args)
     else
        putStrLn "Error: can't find dib.hs." >> return (ExitFailure 255)

main = do
    args <- getArgs
    currentDir <- D.getCurrentDirectory
    exitcode <- findDib "" (intercalate "" args)
    D.setCurrentDirectory currentDir
    return exitcode
