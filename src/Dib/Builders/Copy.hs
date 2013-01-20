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
  D.createDirectoryIfMissing True $ takeDirectory $ T.unpack t
  putStrLn $ "Copying: " ++ T.unpack s ++ " -> " ++ T.unpack t
  D.copyFile (T.unpack s) (T.unpack t)
  return $ Left (OneToOne t "")
copyFunc _ = return $ Right "Unexpected SrcTransform"

remapFile :: String -> SrcTransform -> SrcTransform
remapFile dest (OneToOne s _) = OneToOne s $ T.pack $ replaceDirectory (T.unpack s) dest
remapFile _ _ = error "Unhandled SrcTransform"

makeCopyTarget :: T.Text -> T.Text -> T.Text -> FilterFunc -> Target
makeCopyTarget name src dest f =
  let stage = Stage "copy" (map $ remapFile (T.unpack dest)) return copyFunc 
  in Target name [] [stage] [makeFileTreeGatherer src f]
