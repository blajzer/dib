-- | The Simple builder allows you to execute a 'OneToOne' trasformation
-- given a function that generates a command line from the source and
-- target files.
module Dib.Builders.Simple (
  makeSimpleTarget
  ) where

import Dib.Gatherers
import Dib.Types

import qualified Data.Text as T
import qualified System.Directory as D
import System.Cmd (system)
import System.Exit
import System.FilePath as P

buildFunc :: (String -> String -> String) -> SrcTransform -> IO (Either SrcTransform T.Text)
buildFunc func (OneToOne s t) = do
  let unpackedTarget = T.unpack t
  let unpackedSource = T.unpack s
  D.createDirectoryIfMissing True $ takeDirectory unpackedTarget
  putStrLn $ "Building: " ++ unpackedSource ++ " -> " ++ unpackedTarget
  let buildCmd = func unpackedSource unpackedTarget
  exitCode <- system buildCmd
  handleExitCode exitCode t buildCmd
buildFunc _ _ = return $ Right "Unexpected SrcTransform"

remapFile :: String -> String -> T.Text -> SrcTransform -> SrcTransform
remapFile src dest ext (OneToOne s _) = OneToOne s $ T.pack $ dest </> makeRelative src (T.unpack (changeExt s ext))
remapFile _ _ _ _ = error "Unhandled SrcTransform"

changeExt :: T.Text -> T.Text -> T.Text
changeExt path = T.append (T.dropWhileEnd (/='.') path)

--TODO: move to a utility module and factor out of C builder
handleExitCode :: ExitCode -> T.Text -> String -> IO (Either SrcTransform T.Text)
handleExitCode (ExitSuccess) t _ = return $ Left $ OneToOne t ""
handleExitCode (ExitFailure _) _ e = return $ Right $ T.pack (show e)

-- | The 'makeSimpleTarget' function generates a target.
-- It takes a name, source directory, destination directory, destination extension,
-- gather filter, and a function to build the command line.
makeSimpleTarget :: T.Text -> T.Text -> T.Text -> T.Text -> FilterFunc -> [T.Text] -> (String -> String -> String) -> Target
makeSimpleTarget name src dest ext f extraDeps buildCmdBuilder =
  let stage = Stage name (map $ remapFile (T.unpack src) (T.unpack dest) ext) return extraDeps (buildFunc buildCmdBuilder)
  in Target name (\_ -> 0) [] [stage] [makeFileTreeGatherer src f]
