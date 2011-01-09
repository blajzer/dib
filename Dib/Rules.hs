module Dib.Rules (
    ReplaceExtensionRule(ReplaceExtensionRule),
    ManyToOneRule(ManyToOneRule),
    DummyRule(DummyRule)
    ) where

import Dib
import Data.Maybe
import System.FilePath
import Text.Regex.Base
import Text.Regex.PCRE
import Text.Regex.PCRE.String

-- | Rule that generates OneToOne transforms by replacing the given extension
data ReplaceExtensionRule = ReplaceExtensionRule String String

instance Rule ReplaceExtensionRule where
    evalRule = evalReplaceExtensionRule
    
evalReplaceExtensionRule (ReplaceExtensionRule ext newExt) files =
    let extRegex = "\\." ++ ext ++ "$"
        matchFunc file = if (file =~ extRegex) /= "" then Just (OneToOne file (replaceExtension file newExt)) else Nothing
    in map fromJust $ filter isJust $ map matchFunc files

-- | Rule that just binds all input files into one output file
data ManyToOneRule = ManyToOneRule String

instance Rule ManyToOneRule where
    evalRule = evalManyToOneRule

evalManyToOneRule (ManyToOneRule out) files = [ManyToOne files out]

-- | Rule that just returns a dummy transform
data DummyRule = DummyRule

instance Rule DummyRule where
	evalRule _ _ = [OneToOne "" ""]
