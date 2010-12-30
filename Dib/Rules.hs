module Dib.Rules where

import Dib
import Data.Maybe
import System.FilePath
import Text.Regex.Base
import Text.Regex.PCRE
import Text.Regex.PCRE.String

-- Replaces the first string (the extension) with the second and binds OneToOne
--  also filters out any file not of the desired extension
data ReplaceExtensionRule = ReplaceExtensionRule String String

instance Rule ReplaceExtensionRule where
    evalRule = evalReplaceExtensionRule
    
evalReplaceExtensionRule (ReplaceExtensionRule ext newExt) files =
    let extRegex = "\\." ++ ext ++ "$"
        matchFunc file = if (file =~ extRegex) /= "" then Just (OneToOne file (replaceExtension file newExt)) else Nothing
    in map fromJust $ filter isJust $ map matchFunc files

-- Binds all source files to one output file
data ManyToOneRule = ManyToOneRule String

instance Rule ManyToOneRule where
    evalRule = evalManyToOneRule

evalManyToOneRule (ManyToOneRule out) files = [ManyToOne files out]
