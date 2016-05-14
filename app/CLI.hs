module Main where

import Liquipedia
import Matcher
import Model
import YouTube

import Control.Monad (when, forM_)
import Data.List (find, intercalate)
import Data.Maybe (fromJust, isJust)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= dispatch

dispatch :: [String] -> IO ()
dispatch ("match"   : count : _) | not $ null count = match (read count)
dispatch ("casters" : count : _) | not $ null count = videosWithCasters (read count)
dispatch _ = putStrLn "Unknown action"

-- ========================================================================= --

videosWithCasters :: Int -> IO ()
videosWithCasters count =
  videos <$> listPlaylist uploadsPlaylistId count >>= mapM_ videoWithCasters

match :: Int -> IO ()
match count = do
  allData <- loadData count
  when (isJust allData) $ do
    let dataset@(_, playlist) = fromJust allData
    let ids = map videoId $ videos playlist
    forM_ ids $ attemptMatching dataset

videoWithCasters :: Video -> IO ()
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

uploadsPlaylistId :: YouTubeId
uploadsPlaylistId = "UUHmNTOzvZhZwaRJoioK0Mqw"

loadData :: Int -> IO (Maybe ([Tournament], Playlist))
loadData count = do
  tournaments     <- listTournaments
  playlistContent <- listPlaylist uploadsPlaylistId count
  return $ (,) <$> tournaments <*> Just playlistContent

attemptMatching :: ([Tournament], Playlist) -> YouTubeId -> IO ()
attemptMatching (tournaments, (Playlist details)) id = do
  let someVideo = find ((== id) . videoId) details
  when (isJust someVideo) $ do
    let video = fromJust someVideo
    let matching = matchTournaments tournaments video
    when (isPerfect matching) $ do
      putStr $ videoURL video
      putStr " -> "
      printURL matching

printURL :: Matching -> IO ()
printURL (Perfect tournament) = putStrLn (tournamentURL tournament)
printURL _ = return ()

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
