module Main where

import Liquipedia
import Matcher
import Model
import Youtube

import Data.Maybe (fromJust)
import Data.List (find)
import Control.Monad (when, forM_)

import System.Environment (getEnv)

main :: IO ()
main = do
  apiKey <- getEnv "API_KEY"
  listPlaylistItems apiKey uploadsPlaylistId >>= prettyPrintPlaylistContent

uploadsPlaylistId = "UUHmNTOzvZhZwaRJoioK0Mqw"

loadData :: IO ([Tournament], PlaylistContent)
loadData = do
  apiKey <- getEnv "API_KEY"
  tournaments <- fromJust <$> listTournaments
  playlistContent <- listPlaylistItems apiKey uploadsPlaylistId
  return (tournaments, playlistContent)

attemptMatching :: ([Tournament], PlaylistContent) -> YoutubeId -> IO ()
attemptMatching (tournaments, (PlaylistContent details)) id =
  do
    let video@(VideoDetails title _ description) = fromJust $ find ((== id) . videoId) details
    let matching = matchTournaments video tournaments
    when (not $ isPerfect matching) $ do
      putStrLn title
      prettyPrint matching

matchings :: IO ()
matchings = do
  allData@(_, playlist) <- loadData
  let ids = map videoId $ videoDetails playlist
  forM_ ids (attemptMatching allData)

prettyPrint :: Matching -> IO ()
prettyPrint (Perfect (Tournament t _ _)) = putStrLn $ " PERFECT MATCH: " ++ t
prettyPrint (Approx scores) = do
  putStrLn " APPROX MATCH:"
  putStr . unlines . map prettyPrintApproxScore $ scores
prettyPrint NoMatch = putStrLn " NO MATCH."

prettyPrintApproxScore :: (Score, Tournament) -> String
prettyPrintApproxScore (s, (Tournament t _ _)) = "  " ++ scoreAsPercenage s ++ " " ++ t
  where scoreAsPercenage s = (show $ truncate $ (fromRational s * 100 :: Float)) ++ "%"
