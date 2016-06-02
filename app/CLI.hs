module Main where

import Liquipedia
import Matcher
import Model
import YouTube

import Data.List (intercalate, groupBy)
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
autoPlaylists count = do
  tournamentsWithVideos <- lastTournaments count
  let tournaments = map fst tournamentsWithVideos
  playlists <- createPlaylists tournaments
  mapM_ (\(t, vs) -> mapM_ (\v -> insertVideo (videoId v) (playlistId $ playlists t)) vs) tournamentsWithVideos
  mapM_ printPlaylist (map playlists tournaments)
    where printPlaylist = putStrLn . playlistTitle

lastTournaments :: Int -> IO [(Tournament, [Video])]
lastTournaments count = foundTournaments <$> getDataset count

foundTournaments :: Maybe Dataset -> [(Tournament, [Video])]
foundTournaments  Nothing       = []
foundTournaments (Just dataset) = group tournamentsWithVideos
    where tournamentsWithVideos = catMaybes $ map extractTournament matchings
          matchings = computeMatchings dataset
          group ts = map (\tvs -> (fst . head $ tvs, map snd tvs)) $ groupBy (\p1 p2 -> fst p1 == fst p2) ts
          extractTournament (v, (Perfect t)) = Just (t, v)
          extractTournament  _               = Nothing

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
