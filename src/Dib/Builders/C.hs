-- Copyright (c) 2010-2018 Brett Lajzer
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
import Dib.Util
import Dib.Scanners.CDepScanner

import Data.List as L
import Data.Monoid
import Data.Word
import System.Process (system)
import System.Directory as D
import System.FilePath as F

import qualified Data.Digest.CRC32 as Hash
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified System.Console.ANSI as ANSI

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
  Hash.crc32 $ TE.encodeUtf8 $ T.intercalate "^" [
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
massageFilePath path = T.replace "\\" "_" $ T.replace "/" "_" path

remapObjFile :: BuildLocation -> T.Text -> T.Text
remapObjFile InPlace file = file
remapObjFile (BuildDir dir) file = dir `T.snoc` F.pathSeparator <> massageFilePath file
remapObjFile (ObjAndBinDirs objDir _) file = objDir `T.snoc` F.pathSeparator <> massageFilePath file

remapBinFile :: BuildLocation -> T.Text -> T.Text
remapBinFile InPlace file = file
remapBinFile (BuildDir dir) file = dir `T.snoc` F.pathSeparator <> file
remapBinFile (ObjAndBinDirs _ binDir) file = binDir `T.snoc` F.pathSeparator <> file

-- | Given a 'CTargetInfo', will make the directories required to build the project.
makeBuildDirs :: CTargetInfo -> IO ()
makeBuildDirs info = do
  let helper InPlace = return ()
      helper (BuildDir dir) = D.createDirectoryIfMissing True (T.unpack dir)
      helper (ObjAndBinDirs objDir binDir) = D.createDirectoryIfMissing True (T.unpack objDir) >> D.createDirectoryIfMissing True (T.unpack binDir)
  helper (outputLocation info)
  return ()

excludeFiles :: [T.Text] -> T.Text -> Bool
excludeFiles excl file = L.any (`T.isSuffixOf` file) excl

getCorrectCompileFlags :: CTargetInfo -> T.Text -> T.Text
getCorrectCompileFlags info source = if ".c" `T.isSuffixOf` source then cCompileFlags info else cxxCompileFlags info

-- | Given a 'CTargetInfo', produces a 'Target'
makeCTarget :: CTargetInfo -> Target
makeCTarget info =
  let includeDirString = includeOption info <> T.intercalate (" " <> includeOption info) (includeDirs info)
      makeBuildString source target = T.unpack $ T.concat [compiler info, " ", inFileOption info, " ", source, " ", outFileOption info, " ", target, " ", includeDirString, " ", commonCompileFlags info, " ", getCorrectCompileFlags info source]
      makeLinkString sources target = T.unpack $ T.concat [linker info, " ", T.unwords sources, " ", outFileOption info, " ", target, " ", linkFlags info]
      makeArchiveString sources target = T.unpack $ T.concat [archiver info, " ", archiverFlags info, " ", target, " ", T.unwords sources]

      buildCmd (ManyToOne sources target) = do
        let sourceFile = head sources
        let buildString = makeBuildString sourceFile target
        ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White]
        putStr "Building: "
        ANSI.setSGR [ANSI.Reset]
        putStrLn $ T.unpack sourceFile
        exitCode <- system buildString
        handleExitCode exitCode target buildString
      buildCmd _ = return $ Left "Unhandled SrcTransform."

      linkCmd (ManyToOne sources target) = do
        let linkString = makeLinkString sources target
        ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White]
        putStr "Linking: "
        ANSI.setSGR [ANSI.Reset]
        putStrLn $ T.unpack target
        exitCode <- system linkString
        handleExitCode exitCode target linkString
      linkCmd _ = return $ Left "Unhandled SrcTransform."

      archiveCmd (ManyToOne sources target) = do
        let archiveString = makeArchiveString sources target
        ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White]
        putStr "Archiving: "
        ANSI.setSGR [ANSI.Reset]
        putStrLn $ T.unpack target
        exitCode <- system archiveString
        handleExitCode exitCode target archiveString
      archiveCmd _ = return $ Left "Unhandled SrcTransform."

      buildDirGatherer = makeCommandGatherer $ makeBuildDirs info
      cppStage = Stage "compile" (map (changeExt "o" (outputLocation info))) (cDepScanner (map T.unpack $ includeDirs info)) (extraCompileDeps info) buildCmd
      linkStage = Stage "link" (combineTransforms (remapBinFile (outputLocation info) $ outputName info)) return (extraLinkDeps info) linkCmd
      archiveStage = Stage "archive" (combineTransforms (remapBinFile (outputLocation info) $ outputName info)) return [] archiveCmd
  in Target (targetName info) (cTargetHash info) [] [cppStage, if staticLibrary info then archiveStage else linkStage] [buildDirGatherer, makeFileTreeGatherer (srcDir info) (matchExtensionsExcluded [".cpp", ".c"] [excludeFiles $ exclusions info])]

changeExt :: T.Text -> BuildLocation -> SrcTransform -> SrcTransform
changeExt newExt buildLoc (OneToOne input _) = OneToOne input $ remapObjFile buildLoc $ T.dropWhileEnd (/='.') input <> newExt
changeExt _ _ _ = undefined

combineTransforms :: T.Text -> [SrcTransform] -> [SrcTransform]
combineTransforms target transforms = [ManyToOne (L.sort sources) target]
  where sources = foldl' (\acc (OneToOne input _) -> acc ++ [input]) [] transforms

-- | Given a 'CTargetInfo', produces a 'Target' that will clean the project.
makeCleanTarget :: CTargetInfo -> Target
makeCleanTarget info =
  let cleanCmd (OneToOne input _) = do
        ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White]
        putStr "Removing: "
        ANSI.setSGR [ANSI.Reset]
        putStrLn $ T.unpack input
        D.removeFile (T.unpack input)
        return $ Right $ OneToOne "" ""
      cleanCmd _ = error "Should never hit this."

      objDir InPlace = srcDir info
      objDir (BuildDir dir) = dir
      objDir (ObjAndBinDirs objDir _) = objDir

      programFile InPlace = outputName info
      programFile (BuildDir dir) = dir `T.snoc` F.pathSeparator <> outputName info
      programFile (ObjAndBinDirs _ binDir) = binDir `T.snoc` F.pathSeparator <> outputName info

      cleanStage = Stage "clean" id return [] cleanCmd
      objectGatherer = makeFileTreeGatherer (objDir $ outputLocation info) (matchExtension ".o")
      programGatherer = makeSingleFileGatherer (programFile $ outputLocation info)
  in Target ("clean-" <> targetName info) (const 0) [] [cleanStage] [objectGatherer, programGatherer]
