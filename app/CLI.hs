module Main where

import Liquipedia
import Matcher
import Model
import YouTube

import Control.Monad (mzero)
import Data.List (intercalate)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= dispatch

dispatch :: [String] -> IO ()
dispatch ("casters" : count : _) | present count = videosWithCasters (read count)
dispatch ("match"   : count : _) | present count = match (read count)
dispatch _ = putStrLn "Unknown action"

present :: String -> Bool
present = not.null

videosWithCasters :: Int -> IO ()
videosWithCasters count =
  videos <$> listPlaylist uploadsPlaylistId count >>= mapM_ videoWithCasters

match :: Int -> IO ()
match count = getDataset count >>= printMatchings

printMatchings :: Maybe Dataset -> IO ()
printMatchings (Just dataset) = mapM_ printMatching (computeMatchings dataset)
printMatchings  Nothing       = mzero

computeMatchings :: Dataset -> [(Video, Matching)]
computeMatchings (tournaments, playlist) = map match' (videos playlist)
  where match' v = (v, matchTournaments tournaments v)

printMatching :: (Video, Matching) -> IO ()
printMatching (video, (Perfect tournament)) = do
  putStr $ videoURL video
  putStr " -> "
  putStrLn $ tournamentURL tournament
printMatching _ = mzero

videoWithCasters :: Video -> IO ()
videoWithCasters video = do
  putStrLn $ videoTitle video
  let casters = videoCasters video
  if null casters
  then do
    putStrLn " No caster recognized:"
    putStrLn $ unlines . map (" " ++) . lines $ videoDescription video
  else do
    let pseudos = intercalate ", " $ map casterPseudo casters
    putStrLn $ " " ++ pseudos

uploadsPlaylistId :: YouTubeId
uploadsPlaylistId = "UUHmNTOzvZhZwaRJoioK0Mqw"

type Dataset = ([Tournament], Playlist)

getDataset :: Int -> IO (Maybe Dataset)
getDataset count = do
  tournaments     <- listTournaments
  playlistContent <- pure <$> listPlaylist uploadsPlaylistId count
  return $ (,) <$> tournaments <*> playlistContent
