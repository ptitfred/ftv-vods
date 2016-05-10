module Main where

import Liquipedia
import Matcher
import Model
import YouTube

import Control.Monad (when, forM_)
import Data.List (find, intercalate)
import Data.Maybe (fromJust, isJust)
import System.Environment (getEnv)

main :: IO ()
main = do
  allData <- loadData 200
  when (isJust allData) $ do
    let dataset@(_, playlist) = fromJust allData
    let ids = map videoId $ videoDetails playlist
    forM_ ids $ attemptMatching dataset

videosWithCasters :: Int -> IO ()
videosWithCasters count = do
  apiKey <- getEnv "API_KEY"
  videos <- videoDetails <$> listPlaylistItems apiKey uploadsPlaylistId count
  mapM_ videoWithCasters videos

-- ========================================================================= --

videoWithCasters :: VideoDetails -> IO ()
videoWithCasters videoDetails = do
  putStrLn $ videoTitle videoDetails
  let casters = videoCasters videoDetails
  if null casters
  then do
    putStrLn " No caster recognized:"
    putStrLn $ unlines . map (" " ++) . lines $ videoDescription videoDetails
  else do
    let pseudos = intercalate ", " $ map casterPseudo casters
    putStrLn $ " " ++ pseudos

uploadsPlaylistId :: YoutubeId
uploadsPlaylistId = "UUHmNTOzvZhZwaRJoioK0Mqw"

loadData :: Int -> IO (Maybe ([Tournament], PlaylistContent))
loadData count = do
  apiKey          <- getEnv "API_KEY"
  tournaments     <- listTournaments
  playlistContent <- listPlaylistItems apiKey uploadsPlaylistId count
  return $ (,) <$> tournaments <*> Just playlistContent

attemptMatching :: ([Tournament], PlaylistContent) -> YoutubeId -> IO ()
attemptMatching (tournaments, (PlaylistContent details)) id = do
  let someVideo = find ((== id) . videoId) details
  when (isJust someVideo) $ do
    let video = fromJust someVideo
    let matching = matchTournaments video tournaments
    when (not $ isPerfect matching) $ do
      putStrLn $ videoTitle video
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
