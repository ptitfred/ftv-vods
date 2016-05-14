module Matcher
    ( matchTournaments
    ) where

import Model
import YouTube

import qualified Control.Applicative as A ((<|>))
import Data.List (sortOn)
import Data.Maybe (fromJust, isJust)
import Text.EditDistance
import Text.Parsec

matchTournaments :: [Tournament] -> Video -> Matching
matchTournaments tournaments video = analyze scores
  where scores = bestScores . map (scoreTournament video) $ tournaments
        bestScores = top5 . byScore . positives . map fromJust . filter isJust
        top5 = take 10
        positives = filter ((>0).ofScore)
        byScore = sortOn (negate.ofScore)

analyze :: Scores -> Matching
analyze [] = NoMatch
analyze scores | perfectCount == 1 = Perfect (ofTournament $ head perfects)
               | perfectCount > 1  = Approx perfects
               | otherwise         = Approx scores
  where perfects = filter (isPerfectScore.ofScore) scores
        isPerfectScore score = score == 1
        perfectCount = length perfects

scoreTournament :: Video -> Tournament -> Maybe Scoring
scoreTournament video tournament = with tournament <$> score (tournamentType tournament)
  where Tournament name url _ = tournament
        context = extractContext video
        score Premier  = matchDescription video url A.<|> scoreSentences context name
        score Standard = matchDescription video url
        with = (,)

matchDescription :: Video -> URL -> Maybe Score
matchDescription video url = do
  someURL <- extractURL video
  if (url == someURL)
  then return 1
  else Nothing

scoreSentences :: String -> String -> Maybe Score
scoreSentences s1 s2 = Just $ matchingCount / (toRational $ length ws1)
  where ws1 = words s1
        ws2 = words s2
        matchingCount = toRational $ length $ filter snd $ map (matching ws2) ws1
        matching ws w = (w, closeEnough w ws)

closeEnough :: String -> [String] -> Bool
closeEnough word = any ((<threshold) . levenshtein word)
  where threshold = minimum [3, length word]
        levenshtein = levenshteinDistance defaultEditCosts

extractContext :: Video -> String
extractContext video = fallback title $ parseContext title
  where title = videoTitle video
        fallback v = either (const v) id

extractURL :: Video -> Maybe URL
extractURL = toMaybe . parseURL . videoDescription

toMaybe :: Either a b -> Maybe b
toMaybe = either (const Nothing) Just

parseContext :: String -> Either ParseError String
parseContext input = parse title input input
  where title = context <* many anyChar <* eof
        context = char '[' *> many (noneOf "]") <* char ']'

parseURL :: String -> Either ParseError URL
parseURL input = parse urlExtractor input input
  where urlExtractor = (eof >> return "") <|> urlParser <|> (many (noneOf " \n") *> space *> urlExtractor)
        urlParser = do
          base <- string "http://wiki.teamliquid.net/dota2/"
          path <- many (noneOf " \n")
          return (base ++ path)
