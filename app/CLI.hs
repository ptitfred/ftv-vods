module Main where

import Liquipedia
import Matcher
import Model
import YouTube

import Data.List (intercalate, nub)
import Data.Maybe (catMaybes)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= dispatch

dispatch :: [String] -> IO ()
dispatch ("auto-pls": count : _) | present count = autoPlaylists     (read count)
dispatch ("casters" : count : _) | present count = videosWithCasters (read count)
dispatch ("match"   : count : _) | present count = match             (read count)
dispatch _ = putStrLn "Unknown action"

present :: String -> Bool
present = not.null

autoPlaylists :: Int -> IO ()
autoPlaylists count =
  lastTournaments count >>= createPlaylists >>= mapM_ printPlaylist
    where printPlaylist = putStrLn . playlistTitle

lastTournaments :: Int -> IO [Tournament]
lastTournaments count = foundTournaments <$> getDataset count

foundTournaments :: Maybe Dataset -> [Tournament]
foundTournaments  Nothing       = []
foundTournaments (Just dataset) = nub tournaments
    where tournaments = catMaybes $ map extractTournament matchings
          matchings = map snd $ computeMatchings dataset
          extractTournament (Perfect t) = Just t
          extractTournament  _          = Nothing

videosWithCasters :: Int -> IO ()
videosWithCasters count = do
  pId <- uploadsPlaylistId
  videos <$> listPlaylist pId count >>= mapM_ videoWithCasters

match :: Int -> IO ()
match count = getDataset count >>= printMatchings

printMatchings :: Maybe Dataset -> IO ()
printMatchings (Just dataset) = mapM_ printMatching (computeMatchings dataset)
printMatchings  Nothing       = return ()

computeMatchings :: Dataset -> [(Video, Matching)]
computeMatchings (tournaments, playlist) = map match' (videos playlist)
  where match' v = (v, matchTournaments tournaments v)

printMatching :: (Video, Matching) -> IO ()
printMatching (video, (Perfect tournament)) = do
  putStr $ videoURL video
  putStr " -> "
  putStrLn $ tournamentURL tournament
printMatching _ = return ()

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

uploadsPlaylistId :: IO YouTubeId
uploadsPlaylistId = channelUploadPlaylist <$> findChannel "FroggedTV"

type Dataset = ([Tournament], PlaylistContent)

getDataset :: Int -> IO (Maybe Dataset)
getDataset count = do
  tournaments     <- listTournaments
  pId <- uploadsPlaylistId
  playlistContent <- pure <$> listPlaylist pId count
  return $ (,) <$> tournaments <*> playlistContent
