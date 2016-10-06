-- Copyright (c) 2010-2016 Brett Lajzer
-- See LICENSE for license information.

-- | A builder for C/C++ code.
module Dib.Builders.C (
  CTargetInfo(CTargetInfo, outputName, targetName, srcDir, outputLocation, compiler, linker, archiver, inFileOption, outFileOption, commonCompileFlags, cCompileFlags, cxxCompileFlags, linkFlags, archiverFlags, includeDirs, extraCompileDeps, extraLinkDeps, exclusions, staticLibrary),
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
import Dib.Target
import Dib.Types
import Dib.Scanners.CDepScanner

import Data.List as L
import Data.Word
import System.Process (system)
import System.Directory as D
import System.Exit
import System.FilePath as F

import qualified Data.Digest.CRC32 as Hash
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | The record type that is used to pass configuration info for the C builder.
data CTargetInfo = CTargetInfo {
  -- | The name of the output file.
  outputName :: T.Text,
  -- | The name of the 'Target'. Should be unique among all 'Target's in a given build.
  targetName :: T.Text,
  -- | The directory containing the source for this target.
  srcDir :: T.Text,
  -- | A 'BuildLocation' that defines where the object and executable files go.
  outputLocation :: BuildLocation,
  -- | The compiler executable.
  compiler :: T.Text,
  -- | The linker executable.
  linker :: T.Text,
  -- | The archiver executable.
  archiver :: T.Text,
  -- | The command line option for the input file.
  inFileOption :: T.Text,
  -- | The command line option for the output file.
  outFileOption :: T.Text,
  -- | The compiler's include option
  includeOption :: T.Text,
  -- | Common compiler flags.
  commonCompileFlags :: T.Text,
  -- | C compiler flags.
  cCompileFlags :: T.Text,
  -- | C++ compiler flags.
  cxxCompileFlags :: T.Text,
  -- | Linker flags.
  linkFlags :: T.Text,
  -- | Archiver flags.
  archiverFlags :: T.Text,
  -- | A list of directories where include files can be found. Used for
  -- dependency scanning and automatically appended to the compile line.
  includeDirs :: [T.Text],
  -- | Extra compilation dependencies.
  extraCompileDeps :: [T.Text],
  -- | Extra linking dependencies.
  extraLinkDeps :: [T.Text],
  -- | Files to exclude from the build.
  exclusions :: [T.Text],
  -- | Whether or not to build a static lib (using the archiver)
  staticLibrary :: Bool
  }

-- | Given a 'CTargetInfo' and a 'Target', produces a checksum
cTargetHash :: CTargetInfo -> Target -> Word32
cTargetHash info _ =
  let textHash = TE.encodeUtf8 $ T.intercalate "^" [
        "srcDir",
        srcDir info,
        "compiler",
        compiler info,
        "linker",
        linker info,
        "archiver",
        archiver info,
        "inFileOption",
        inFileOption info,
        "outFileOption",
        outFileOption info,
        "includeOption",
        includeOption info,
        "commonCompileFlags",
        commonCompileFlags info,
        "cCompileFlags",
        cCompileFlags info,
        "cxxCompileFlags",
        cxxCompileFlags info,
        "linkFlags",
        linkFlags info,
        "archiverFlags",
        archiverFlags info,
        "includeDirs",
        T.intercalate "^^" $ includeDirs info,
        "extraCompileDeps",
        T.intercalate "^^" $ extraCompileDeps info,
        "extraLinkDeps",
        T.intercalate "^^" $ extraLinkDeps info,
        "exclusions",
        T.intercalate "^^" $ exclusions info,
        "staticLibrary",
        if staticLibrary info then "True" else "False"
        ]

  in Hash.crc32 textHash

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
  outputName = "",
  targetName = "",
  srcDir = "",
  outputLocation = InPlace,
  compiler = "",
  linker = "",
  archiver = "",
  inFileOption = "",
  outFileOption = "",
  includeOption = "",
  commonCompileFlags = "",
  cCompileFlags = "",
  cxxCompileFlags = "",
  linkFlags = "",
  archiverFlags = "",
  includeDirs = [],
  extraCompileDeps = [],
  extraLinkDeps = [],
  exclusions = [],
  staticLibrary = False
  }

-- | A default configuration for gcc.
defaultGCCConfig :: CTargetInfo
defaultGCCConfig = emptyConfig {
  compiler = "gcc",
  linker = "gcc",
  archiver = "ar",
  inFileOption = "-c",
  outFileOption = "-o",
  includeOption = "-I",
  archiverFlags = "rs"
  }

-- | A default configuration for g++.
defaultGXXConfig :: CTargetInfo
defaultGXXConfig = defaultGCCConfig {
  compiler = "gcc",
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
  let helper InPlace = return ()
      helper (BuildDir d) = D.createDirectoryIfMissing True (T.unpack d)
      helper (ObjAndBinDirs d d2) = D.createDirectoryIfMissing True (T.unpack d) >> D.createDirectoryIfMissing True (T.unpack d2)
  helper (outputLocation info)
  return ()

excludeFiles :: [T.Text] -> T.Text -> Bool
excludeFiles excl file = L.any (`T.isSuffixOf` file) excl

getCorrectCompileFlags :: CTargetInfo -> T.Text -> T.Text
getCorrectCompileFlags info s = if ".c" `T.isSuffixOf` s then cCompileFlags info else cxxCompileFlags info

-- | Given a 'CTargetInfo', produces a 'Target'
makeCTarget :: CTargetInfo -> Target
makeCTarget info =
  let includeDirString = includeOption info `T.append` T.intercalate (" " `T.append` includeOption info) (includeDirs info)
      makeBuildString s t = T.unpack $ T.concat [compiler info, " ", inFileOption info, " ", s, " ", outFileOption info, " ", t, " ", includeDirString, " ", commonCompileFlags info, " ", getCorrectCompileFlags info s]
      makeLinkString ss t = T.unpack $ T.concat [linker info, " ", T.unwords ss, " ", outFileOption info, " ", t, " ", linkFlags info]
      makeArchiveString ss t = T.unpack $ T.concat [archiver info, " ", archiverFlags info, " ", t, " ", T.unwords ss]

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

      archiveCmd (ManyToOne ss t) = do
        let archiveString = makeArchiveString ss t
        putStrLn $ "Archiving: " ++ T.unpack t
        exitCode <- system archiveString
        handleExitCode exitCode t archiveString
      archiveCmd _ = return $ Right "Unhandled SrcTransform."

      buildDirGatherer = makeCommandGatherer $ makeBuildDirs info
      cppStage = Stage "compile" (map (changeExt "o" (outputLocation info))) (cDepScanner (map T.unpack $ includeDirs info)) (extraCompileDeps info) buildCmd
      linkStage = Stage "link" (combineTransforms (remapBinFile (outputLocation info) $ outputName info)) return (extraLinkDeps info) linkCmd
      archiveStage = Stage "archive" (combineTransforms (remapBinFile (outputLocation info) $ outputName info)) return [] archiveCmd
  in Target (targetName info) (cTargetHash info) [] [cppStage, if staticLibrary info then archiveStage else linkStage] [buildDirGatherer, makeFileTreeGatherer (srcDir info) (matchExtensionsExcluded [".cpp", ".c"] [excludeFiles $ exclusions info])]

changeExt :: T.Text -> BuildLocation -> SrcTransform -> SrcTransform
changeExt newExt b (OneToOne l _) = OneToOne l $ remapObjFile b $ T.append (T.dropWhileEnd (/='.') l) newExt
changeExt _ _ _ = undefined

handleExitCode :: ExitCode -> T.Text -> String -> IO (Either SrcTransform T.Text)
handleExitCode ExitSuccess t _ = return $ Left $ OneToOne t ""
handleExitCode (ExitFailure _) _ e = return $ Right $ T.pack (show e)

combineTransforms :: T.Text -> [SrcTransform] -> [SrcTransform]
combineTransforms t st = [ManyToOne sources t]
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

      programFile InPlace = outputName info
      programFile (BuildDir d) = d `T.snoc` F.pathSeparator `T.append` outputName info
      programFile (ObjAndBinDirs _ d) = d `T.snoc` F.pathSeparator `T.append` outputName info

      cleanStage = Stage "clean" id return [] cleanCmd
      objectGatherer = makeFileTreeGatherer (objDir $ outputLocation info) (matchExtension ".o")
      programGatherer = makeSingleFileGatherer (programFile $ outputLocation info)
  in Target ("clean-" `T.append` targetName info) (const 0) [] [cleanStage] [objectGatherer, programGatherer]
