module Matcher
    ( matchTournaments, context, title
    ) where

import Data.List (sortOn)
import Model
import Text.EditDistance
import Text.Parsec

matchTournaments :: VideoDetails -> [Tournament] -> [(Score, Tournament)]
matchTournaments video = bestScores . map matchTournament
  where matchTournament = scoreTournament (extractContext video)
        bestScores = top5 . byScore . positives
        top5 = take 5
        positives = filter ((>0).fst)
        byScore = sortOn (negate.fst)

scoreTournament :: String -> Tournament -> (Score, Tournament)
scoreTournament title tournament = (score, tournament)
  where Tournament name _ = tournament
        score = scoreSentences title name

scoreSentences :: String -> String -> Score
scoreSentences s1 s2 = matchingCount / (toRational $ length ws1)
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
  where VideoDetails title _ = video
        fallback v = either (const v) id

context = char '[' *> many (noneOf "]") <* char ']'
title = context <* many anyChar <* eof

parseContext :: String -> Either ParseError String
parseContext input = parse title input input
