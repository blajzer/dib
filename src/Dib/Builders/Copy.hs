module Dib.Builders.Copy (
  makeCopyTarget
  ) where

import Dib.Gatherers
import Dib.Types

import qualified Data.Text as T
import qualified System.Directory as D
import System.FilePath as P

copyFunc :: SrcTransform -> IO (Either SrcTransform T.Text)
copyFunc (OneToOne s t) = do
  let unpackedTarget = T.unpack t
  let unpackedSource = T.unpack s
  D.createDirectoryIfMissing True $ takeDirectory $ unpackedTarget
  putStrLn $ "Copying: " ++ unpackedSource ++ " -> " ++ unpackedTarget
  D.copyFile unpackedSource unpackedTarget
  return $ Left (OneToOne t "")
copyFunc _ = return $ Right "Unexpected SrcTransform"

remapFile :: String -> String -> SrcTransform -> SrcTransform
remapFile src dest (OneToOne s _) = OneToOne s $ T.pack $ dest </> (makeRelative src (T.unpack s))
remapFile _ _ _ = error "Unhandled SrcTransform"

makeCopyTarget :: T.Text -> T.Text -> T.Text -> FilterFunc -> Target
makeCopyTarget name src dest f =
  let stage = Stage "copy" (map $ remapFile (T.unpack src) (T.unpack dest)) return copyFunc 
  in Target name [] [stage] [makeFileTreeGatherer src f]
