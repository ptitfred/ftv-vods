module Matcher
    ( matchTournaments
    ) where

import qualified Control.Applicative as A ((<|>))
import Data.List (sortOn)
import Data.Maybe (fromJust, isJust)
import Model
import Text.EditDistance
import Text.Parsec

matchTournaments :: VideoDetails -> [Tournament] -> Matching
matchTournaments video tournaments = analyze scores
  where scores = bestScores . map (scoreTournament video) $ tournaments
        bestScores = top5 . byScore . positives . map fromJust . filter isJust
        top5 = take 10
        positives = filter ((>0).fst)
        byScore = sortOn (negate.fst)

analyze :: Scores -> Matching
analyze [] = NoMatch
analyze scores | perfectCount == 1 = Perfect (snd $ head perfects)
               | perfectCount > 1  = Approx perfects
               | otherwise         = Approx scores
  where perfects = filter (\(s, t) -> s == 1) scores
        perfectCount = length perfects

scoreTournament :: VideoDetails -> Tournament -> Maybe (Score, Tournament)
scoreTournament video tournament = (\s -> (s, tournament)) <$> score tournamentType
  where Tournament name url tournamentType = tournament
        context = extractContext video
        score Premier = matchDescription video url A.<|> scoreSentences context name
        score Standard     = matchDescription video url

matchDescription :: VideoDetails -> URL -> Maybe Score
matchDescription video url = do
  someURL <- extractURL video
  if url == someURL then return 1 else Nothing

scoreSentences :: String -> String -> Maybe Score
scoreSentences s1 s2 = Just $ matchingCount / (toRational $ length ws1)
  where ws1 = words s1
        ws2 = words s2
        matchingCount = toRational $ length $ filter snd $ map (matching ws2) ws1
        matching words w = (w, closeEnough w words)

closeEnough :: String -> [String] -> Bool
closeEnough word = any ((<threshold) . levenshtein word)
  where threshold = minimum [3, length word]
        levenshtein = levenshteinDistance defaultEditCosts

extractContext :: VideoDetails -> String
extractContext video = fallback title $ parseContext title
  where VideoDetails title _ _ = video
        fallback v = either (const v) id

extractURL :: VideoDetails -> Maybe URL
extractURL video = case parseURL description of
   Right u -> Just u
   Left _  -> Nothing
  where VideoDetails _ _ description = video

context = char '[' *> many (noneOf "]") <* char ']'
title = context <* many anyChar <* eof

parseContext :: String -> Either ParseError String
parseContext input = parse title input input

parseURL :: String -> Either ParseError URL
parseURL input = parse urlExtractor input input

urlExtractor = (eof >> return "") <|> url <|> (many (noneOf " \n") *> space *> urlExtractor)

url = do
  (++) <$> string "http://wiki.teamliquid.net/dota2/" <*> many (noneOf " \n")