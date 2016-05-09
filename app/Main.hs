module Main where

import Liquipedia
import Matcher
import Model
import Youtube

import Control.Monad (when, forM_)
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import System.Environment (getEnv)

main :: IO ()
main = do
  allData <- loadData
  when (isJust allData) $ do
    let dataset@(_,playlist) = fromJust allData
    let ids = map videoId $ videoDetails playlist
    forM_ ids $ attemptMatching dataset

uploadsPlaylistId :: YoutubeId
uploadsPlaylistId = "UUHmNTOzvZhZwaRJoioK0Mqw"

loadData :: IO (Maybe ([Tournament], PlaylistContent))
loadData = do
  apiKey          <- getEnv "API_KEY"
  tournaments     <- listTournaments
  playlistContent <- listPlaylistItems apiKey uploadsPlaylistId
  return $ (,) <$> tournaments <*> Just playlistContent

attemptMatching :: ([Tournament], PlaylistContent) -> YoutubeId -> IO ()
attemptMatching (tournaments, (PlaylistContent details)) id = do
  let someVideo = find ((== id) . videoId) details
  when (isJust someVideo) $ do
    let video@(VideoDetails title _ description) = fromJust someVideo
    let matching = matchTournaments video tournaments
    when (not $ isPerfect matching) $ do
      putStrLn title
      prettyPrint matching

prettyPrint :: Matching -> IO ()
prettyPrint NoMatch =
  putStrLn " NO MATCH."
prettyPrint (Perfect tournament) =
  putStrLn $ " PERFECT MATCH: " ++ (tournamentName tournament)
prettyPrint (Approx scores) = do
  putStrLn " APPROX MATCH:"
  putStr . unlines . map prettyPrintApproxScore $ scores

prettyPrintApproxScore :: Scoring -> String
prettyPrintApproxScore scoring = "  " ++ scoreAsPercentage s ++ " " ++ t
  where s = ofScore scoring
        t = tournamentName $ ofTournament scoring

scoreAsPercentage :: Score -> String
scoreAsPercentage score = (show $ truncate $ percents score) ++ "%"

percents :: Score -> Float
percents score = fromRational score * 100
