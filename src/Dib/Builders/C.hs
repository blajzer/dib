-- | A builder for C/C++ code.
module Dib.Builders.C (
  CTargetInfo(CTargetInfo, projectName, srcDir, outputLocation, compiler, linker, inFileOption, outFileOption, compileFlags, linkFlags, includeDirs, extraCompileDeps, extraLinkDeps),
  BuildLocation(InPlace, BuildDir, ObjAndBinDirs),
  makeCTarget,
  makeCleanTarget,
  makeBuildDirs,
  emptyConfig,
  defaultGCCConfig,
  defaultGXXConfig,
  defaultClangConfig
  ) where

import Dib.Gatherers
import Dib.Types
import Dib.Scanners.CDepScanner

import Data.List as L
import qualified Data.Text as T
import System.Cmd (system)
import System.Directory as D
import System.Exit
import System.FilePath as F

-- | The record type that is used to pass configuration info for the C builder.
data CTargetInfo = CTargetInfo {
  -- | The name of the project.
  projectName :: T.Text,
  -- | The directory containing the source for this target.
  srcDir :: T.Text,
  -- | A 'BuildLocation' that defines where the object and executable files go.
  outputLocation :: BuildLocation,
  -- | The compiler executable.
  compiler :: String,
  -- | The linker executable.
  linker :: String,
  -- | The command line option for the input file.
  inFileOption :: String,
  -- | The command line option for the output file.
  outFileOption :: String,
  -- | Compiler flags.
  compileFlags :: String,
  -- | Linker flags.
  linkFlags :: String,
  -- | A list of directories where include files can be found. Used for
  -- dependency scanning.
  includeDirs :: [String],
  -- | Extra compilation dependencies.
  extraCompileDeps :: [T.Text],
  -- | Extra linking dependencies.
  extraLinkDeps :: [T.Text]
  }

-- | The data type for specifying where built files end up.
data BuildLocation =
  -- | Specifies that object files will end up adjacent to their source files
  -- and the executable will be in the same directory as the dib.hs file.
  InPlace
  -- | Specifies that the object files and executable will go in a certain directory.
  | BuildDir T.Text
  -- | Specifies that the object files will go in the first directory and the
  -- executable in the second directory.
  | ObjAndBinDirs T.Text T.Text

-- | An empty configuration.
emptyConfig :: CTargetInfo
emptyConfig = CTargetInfo {
  projectName = "",
  srcDir = "",
  outputLocation = InPlace,
  compiler = "",
  linker = "",
  inFileOption = "",
  outFileOption = "",
  compileFlags = "",
  linkFlags = "",
  includeDirs = [],
  extraCompileDeps = [],
  extraLinkDeps = []
  }

-- | A default configuration for gcc.
defaultGCCConfig :: CTargetInfo
defaultGCCConfig = emptyConfig {
  compiler = "gcc",
  linker = "gcc",
  inFileOption = "-c",
  outFileOption = "-o"
  }

-- | A default configuration for g++.
defaultGXXConfig :: CTargetInfo
defaultGXXConfig = defaultGCCConfig {
  compiler = "g++",
  linker = "g++"
  }

-- | A default configuration for clang.
defaultClangConfig :: CTargetInfo
defaultClangConfig = defaultGCCConfig {
  compiler = "clang",
  linker = "clang"
  }

massageFilePath :: T.Text -> T.Text
massageFilePath p = T.replace "\\" "_" $ T.replace "/" "_" p

remapObjFile :: BuildLocation -> T.Text -> T.Text
remapObjFile InPlace f = f
remapObjFile (BuildDir d) f = d `T.snoc` F.pathSeparator `T.append` massageFilePath f
remapObjFile (ObjAndBinDirs d _) f = d `T.snoc` F.pathSeparator `T.append` massageFilePath f 

remapBinFile :: BuildLocation -> T.Text -> T.Text
remapBinFile InPlace f = f
remapBinFile (BuildDir d) f = d `T.snoc` F.pathSeparator `T.append` f
remapBinFile (ObjAndBinDirs _ d) f = d `T.snoc` F.pathSeparator `T.append` f

-- | Given a 'CTargetInfo', will make the directories required to build the project.
makeBuildDirs :: CTargetInfo -> IO ()
makeBuildDirs info = do
  let helper (InPlace) = return ()
      helper (BuildDir d) = D.createDirectoryIfMissing True (T.unpack d)
      helper (ObjAndBinDirs d d2) = D.createDirectoryIfMissing True (T.unpack d) >> D.createDirectoryIfMissing True (T.unpack d2)
  helper (outputLocation info)
  return ()

-- | Given a 'CTargetInfo', produces a 'Target'
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

      buildDirGatherer = makeCommandGatherer $ makeBuildDirs info
      cppStage = Stage "compile" (map (changeExt "o" (outputLocation info))) (cDepScanner (includeDirs info) (extraCompileDeps info)) buildCmd
      linkStage = Stage "link" (combineTransforms (remapBinFile (outputLocation info) $ projectName info) (extraLinkDeps info)) return linkCmd
  in Target (projectName info) [] [cppStage, linkStage] [buildDirGatherer, makeFileTreeGatherer (srcDir info) (matchExtensions [".cpp", ".c"])]

changeExt :: T.Text -> BuildLocation -> SrcTransform -> SrcTransform
changeExt newExt b (OneToOne l _) = OneToOne l $ remapObjFile b $ T.append (T.dropWhileEnd (/='.') l) newExt
changeExt _ _ _ = undefined

handleExitCode :: ExitCode -> T.Text -> String -> IO (Either SrcTransform T.Text)
handleExitCode (ExitSuccess) t _ = return $ Left $ OneToOne t ""
handleExitCode (ExitFailure _) _ e = return $ Right $ T.pack (show e)

combineTransforms :: T.Text -> [T.Text] -> [SrcTransform] -> [SrcTransform]
combineTransforms t extraDeps st = [ManyToOne (sources ++ extraDeps) t]
  where sources = foldl' (\l (OneToOne s _) -> l ++ [s]) [] st

-- | Given a 'CTargetInfo', produces a 'Target' that will clean the project.
makeCleanTarget :: CTargetInfo -> Target
makeCleanTarget info =
  let cleanCmd (OneToOne s _) = do
        putStrLn $ "removing: " ++ T.unpack s
        D.removeFile (T.unpack s)
        return $ Left $ OneToOne "" ""
      cleanCmd _ = error "Should never hit this."
  
      objDir InPlace = srcDir info
      objDir (BuildDir d) = d
      objDir (ObjAndBinDirs d _) = d
      
      programFile InPlace = projectName info
      programFile (BuildDir d) = d `T.snoc` F.pathSeparator `T.append` projectName info
      programFile (ObjAndBinDirs _ d) = d `T.snoc` F.pathSeparator `T.append` projectName info
  
      cleanStage = Stage "clean" id return cleanCmd
      objectGatherer = makeFileTreeGatherer (objDir $ outputLocation info) (matchExtension ".o")
      programGatherer = makeSingleFileGatherer (programFile $ outputLocation info)
  in Target ("clean-" `T.append` projectName info) [] [cleanStage] [objectGatherer, programGatherer]

