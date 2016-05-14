module Helpers
    ( extractCasters
    ) where

import Model

import Data.Char (isAlphaNum)
import Data.List (find)
import Data.List.Split (wordsBy)
import Data.Maybe (catMaybes)

extractCasters :: String -> [Caster]
extractCasters description = catMaybes $ map nameToCaster $ filter (`elem` (concatMap casterPseudos casters)) $ tokenize description

tokenize :: String -> [String]
tokenize = wordsBy sep
  where sep = not.isAlphaNum

nameToCaster :: Name -> Maybe Caster
nameToCaster name = find (isCaster name) casters
