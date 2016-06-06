module Helpers
    ( Parenting
    , decorate
    , extractCasters
    , hashURL
    , openBrowser
    , parents
    ) where

import Model

import Control.Monad              (void)
import Crypto.Hash                (Digest, SHA1, hash)
import Data.ByteString            (ByteString)
import Data.ByteString.Char8 as C (pack)
import Data.Char                  (isAlphaNum)
import Data.List                  (find, intercalate)
import Data.List.Split            (splitOn, wordsBy)
import Data.Map.Lazy              (fromList, (!))
import Data.Maybe                 (catMaybes, fromMaybe)
import System.Process             (createProcess, proc, StdStream(CreatePipe), std_out, std_err)

extractCasters :: [Caster] -> String -> [Caster]
extractCasters casters description = catMaybes $ map (nameToCaster casters) $ filter (`elem` (concatMap casterPseudos casters)) $ tokenize description

tokenize :: String -> [String]
tokenize = wordsBy sep
  where sep = not.isAlphaNum

nameToCaster :: [Caster] -> Name -> Maybe Caster
nameToCaster casters name = find (isCaster name) casters

openBrowser :: URL -> IO ()
openBrowser url = do
  void $ createProcess (proc "xdg-open" [url]) { std_out = CreatePipe
                                               , std_err = CreatePipe
                                               }

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

carry :: Ord a => [(a, b)] -> (a -> b)
carry = (!) . fromList

parent :: URL -> URL
parent = intercalate "/" . reverse . tail . reverse . splitOn "/"
