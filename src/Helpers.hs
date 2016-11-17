module Helpers
    ( Parenting
    , decorate
    , extractCasters
    , hashURL
    , parents
    ) where

import Model

import Crypto.Hash                (Digest, SHA1, hash)
import Data.ByteString            (ByteString)
import Data.ByteString.Char8 as C (pack)
import Data.Char                  (isSpace)
import Data.List                  (find, intercalate)
import Data.List.Split            (splitOn, wordsBy)
import Data.Map.Lazy              (fromList, (!))
import Data.Maybe                 (catMaybes, fromMaybe)

extractCasters :: [Caster] -> String -> [Caster]
extractCasters casters description = catMaybes knownCasters
  where knownCasters = map (nameToCaster casters) knownNames
        knownNames   = filter (`elem` names) tokens
        tokens       = tokenize description
        names        = concatMap casterPseudos casters

tokenize :: String -> [String]
tokenize = wordsBy isSpace

nameToCaster :: [Caster] -> Name -> Maybe Caster
nameToCaster casters name = find (isCaster name) casters

hashURL :: URL -> String
hashURL = show . sha1 . C.pack

sha1 :: ByteString -> Digest SHA1
sha1 = hash

type Parenting = Tournament -> Tournament

parents :: [Tournament] -> Parenting
parents ts = carry $ decorate (findParent ts) ts

findParent :: [Tournament] -> Tournament -> Tournament
findParent ts t = fromMaybe t $ find (\t' -> tournamentURL t' == parentURL t) ts
  where parentURL = parent . tournamentURL

decorate :: (a -> b) -> [a] -> [(a, b)]
decorate f = map (\x -> (x, f x))

carry :: Ord a => [(a, b)] -> a -> b
carry = (!) . fromList

parent :: URL -> URL
parent = intercalate "/" . init . splitOn "/"
