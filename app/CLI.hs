module Main where

import Liquipedia
import Matcher
import Model
import YouTube

import Data.List (intercalate, groupBy, (\\))
import Data.Maybe (catMaybes)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= runClient.dispatch

dispatch :: [String] -> Client ()
dispatch ("auto-pls": count : _) | present count = autoPlaylists     (read count)
dispatch ("casters" : count : _) | present count = videosWithCasters (read count)
dispatch ("match"   : count : _) | present count = match             (read count)
dispatch _ = liftIO $ putStrLn "Unknown action"

present :: String -> Bool
present = not.null

autoPlaylists :: Int -> Client ()
autoPlaylists count = do
  tournamentsWithVideos <- lastTournaments count
  let tournaments = map fst tournamentsWithVideos
  playlists <- createPlaylists tournaments
  tournamentsWithNewVideos <- onlyNewVideos playlists tournamentsWithVideos
  let items = [(v, playlists t) | (t, vs) <- tournamentsWithNewVideos, v <- vs]
  mapM_ (uncurry insertVideo) items
  mapM_ (liftIO.printPlaylist) (map playlists tournaments)
    where printPlaylist = putStrLn.playlistTitle

onlyNewVideos :: (Tournament -> Playlist) -> [Serie] -> Client [Serie]
onlyNewVideos ps = mapM (filterOldVideos ps)

type Serie = (Tournament, [Video])

filterOldVideos :: (Tournament -> Playlist) -> Serie -> Client Serie
filterOldVideos playlists (tournament, vs) = do
  oldVideos <- videos <$> listPlaylist (playlistId $ playlists tournament) 1000
  return (tournament, vs \\ oldVideos)

lastTournaments :: Int -> Client [Serie]
lastTournaments count = foundTournaments <$> getDataset count

foundTournaments :: Maybe Dataset -> [Serie]
foundTournaments  Nothing       = []
foundTournaments (Just dataset) = group tournamentsWithVideos
  where tournamentsWithVideos = catMaybes $ map extractTournament matchings
        matchings = computeMatchings dataset
        group ts = map (\tvs -> (fst . head $ tvs, map snd tvs)) $ groupBy (\p1 p2 -> fst p1 == fst p2) ts
        extractTournament (v, (Perfect t)) = Just (t, v)
        extractTournament  _               = Nothing

videosWithCasters :: Int -> Client ()
videosWithCasters count = do
  pId <- uploadsPlaylistId
  videos <$> listPlaylist pId count >>= mapM_ (liftIO.videoWithCasters)

match :: Int -> Client ()
match count = getDataset count >>= liftIO.printMatchings

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

uploadsPlaylistId :: Client YouTubeId
uploadsPlaylistId = channelUploadPlaylist <$> findChannel "FroggedTV"

type Dataset = ([Tournament], PlaylistContent)

getDataset :: Int -> Client (Maybe Dataset)
getDataset count = do
  tournaments <- liftIO listTournaments
  pId <- uploadsPlaylistId
  playlistContent <- pure <$> listPlaylist pId count
  return $ (,) <$> tournaments <*> playlistContent
