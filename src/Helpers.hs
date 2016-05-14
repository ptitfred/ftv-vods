module Helpers
    ( extractCasters
    ) where

import Model

import Data.Char (isAlphaNum)
import Data.List (find)
import Data.List.Split (wordsBy)
import Data.Maybe (catMaybes)

extractCasters :: [Caster] -> String -> [Caster]
extractCasters casters description = catMaybes $ map (nameToCaster casters) $ filter (`elem` (concatMap casterPseudos casters)) $ tokenize description

tokenize :: String -> [String]
tokenize = wordsBy sep
  where sep = not.isAlphaNum

nameToCaster :: [Caster] -> Name -> Maybe Caster
nameToCaster casters name = find (isCaster name) casters
