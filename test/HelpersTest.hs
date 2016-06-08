module HelpersTest (suite) where

import Helpers
import Model

import Data.List (intercalate)

import Test.QuickCheck
import Test.QuickCheck.Modifiers

import Test.Tasty
import Test.Tasty.QuickCheck as QC

instance Eq Caster where
  c1 == c2 = casterPseudo c1 == casterPseudo c2

genCasterName :: Gen Name
genCasterName = listOf1 $ elements alphaNums

alphaNums :: [Char]
alphaNums = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

newtype WhitespaceChar = WhitespaceChar Char deriving (Eq, Show)

instance Arbitrary WhitespaceChar where
  arbitrary = WhitespaceChar <$> elements " \n\t\r"

instance Arbitrary Caster where
  arbitrary = do
    pseudo  <- genCasterName
    aliases <- listOf genCasterName
    return $ Caster pseudo [] Nothing

prop_extractCasters_id :: WhitespaceChar -> [Caster] -> Bool
prop_extractCasters_id (WhitespaceChar sep) casters | not $ sep `elem` alphaNums = extractCasters casters text == casters
  where text = join sep casters
        join sep casters = intercalate (sep:[]) $ map casterPseudo casters

suite :: TestTree
suite = testGroup "Helpers"
  [ QC.testProperty "extract casters" prop_extractCasters_id
  ]
