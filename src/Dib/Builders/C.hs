module Dib.Builders.C (
  CTargetInfo(CTargetInfo, projectName, srcDir, compiler, linker, inFileOption, outFileOption, compileFlags, linkFlags, includeDirs),
  makeCTarget,
  makeCleanTarget
  ) where

import Dib.Gatherers
import Dib.Types
import Dib.Scanners.CDepScanner

import Data.List as L
import qualified Data.Text as T
import System.Cmd (system)
import System.Directory as D
import System.Exit

data CTargetInfo = CTargetInfo {
  projectName :: T.Text,
  srcDir :: T.Text,
  compiler :: String,
  linker :: String,
  inFileOption :: String,
  outFileOption :: String,
  compileFlags :: String,
  linkFlags :: String,
  includeDirs :: [String]
  }

makeCTarget :: CTargetInfo -> Target
makeCTarget info = 
  let makeBuildString s t = compiler info ++ " " ++ inFileOption info ++ " " ++ T.unpack s ++ " " ++ outFileOption info ++ " " ++ T.unpack t ++ " " ++ compileFlags info
      makeLinkString ss t = linker info ++ " " ++ unwords (map T.unpack ss) ++ " " ++ outFileOption info ++ " " ++ T.unpack t ++ " " ++ linkFlags info

      buildCmd (ManyToOne ss t) = do
        let sourceFile = head ss
        let buildString = makeBuildString sourceFile t
        putStrLn $ "Building: " ++ T.unpack sourceFile
        exitCode <- system buildString
        handleExitCode exitCode t buildString
      buildCmd _ = return $ Right "Unhandled SrcTransform."

      linkCmd (ManyToOne ss t) = do
        let linkString = makeLinkString ss t
        putStrLn $ "Linking: " ++ T.unpack t
        exitCode <- system linkString
        handleExitCode exitCode t linkString
      linkCmd _ = return $ Right "Unhandled SrcTransform."

      cppStage = Stage "compile" (map (changeExt "o")) (cDepScanner (includeDirs info)) buildCmd
      linkStage = Stage "link" (combineTransforms (projectName info)) return linkCmd
  in Target (projectName info) [] [cppStage, linkStage] [makeFileTreeGatherer (srcDir info) (matchExtensions [".cpp", ".c"])]

changeExt :: T.Text -> SrcTransform -> SrcTransform
changeExt newExt (OneToOne l _) = OneToOne l $ T.append (T.dropWhileEnd (/='.') l) newExt
changeExt _ _ = undefined

handleExitCode :: ExitCode -> T.Text -> String -> IO (Either SrcTransform T.Text)
handleExitCode (ExitSuccess) t _ = return $ Left $ OneToOne t ""
handleExitCode (ExitFailure _) _ e = return $ Right $ T.pack (show e)

combineTransforms :: T.Text -> [SrcTransform] -> [SrcTransform]
combineTransforms t st = [ManyToOne sources t]
  where sources = foldl' (\l (OneToOne s _) -> l ++ [s]) [] st

makeCleanTarget :: CTargetInfo -> Target
makeCleanTarget info =
  let cleanCmd (OneToOne s _) = do
        putStrLn $ "removing: " ++ T.unpack s
        D.removeFile (T.unpack s)
        return $ Left $ OneToOne "" ""
        
      cleanStage = Stage "clean" id return cleanCmd
      objectGatherer = makeFileTreeGatherer (srcDir info) (matchExtension ".o")
      programGatherer = makeSingleFileGatherer (projectName info)
  in Target "clean" [] [cleanStage] [objectGatherer, programGatherer]

