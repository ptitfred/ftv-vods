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
  apiKey <- getEnv "API_KEY"
  tournaments <- listTournaments
  playlistContent <- listPlaylistItems apiKey uploadsPlaylistId
  return $ (,) <$> tournaments <*> Just playlistContent

attemptMatching :: ([Tournament], PlaylistContent) -> YoutubeId -> IO ()
attemptMatching (tournaments, (PlaylistContent details)) id =
  do
    let someVideo = find ((== id) . videoId) details
    when (isJust someVideo) $ do
      let video@(VideoDetails title _ description) = fromJust someVideo
      let matching = matchTournaments video tournaments
      when (not $ isPerfect matching) $ do
        putStrLn title
        prettyPrint matching

prettyPrint :: Matching -> IO ()
prettyPrint NoMatch = putStrLn " NO MATCH."
prettyPrint (Perfect (Tournament t _ _)) = putStrLn $ " PERFECT MATCH: " ++ t
prettyPrint (Approx scores) = do
  putStrLn " APPROX MATCH:"
  putStr . unlines . map prettyPrintApproxScore $ scores

prettyPrintApproxScore :: (Score, Tournament) -> String
prettyPrintApproxScore (s, (Tournament t _ _)) = "  " ++ scoreAsPercenage s ++ " " ++ t
  where scoreAsPercenage s = (show $ truncate $ (fromRational s * 100 :: Float)) ++ "%"
