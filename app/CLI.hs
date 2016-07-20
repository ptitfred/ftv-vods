module Main where

import Helpers (decorate)
import Liquipedia
import Matcher
import Model
import YouTube
import PlaylistManager

import Control.Arrow      ((&&&))
import Control.Monad      (forM_)
import Data.List          (intercalate, groupBy, sortOn)
import Data.Maybe         (mapMaybe)
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
  tournamentsWithVideos <- reverse <$> lastTournaments count
  let tournaments = map fst tournamentsWithVideos
  playlists <- createPlaylists tournaments
  updates <- onlyNewVideos playlists tournamentsWithVideos
  forM_ updates (updateTournament playlists)

type Serie       = (Tournament, [Video])
type SerieUpdate = (Tournament, [(Video, Position)])

updateTournament :: (Tournament -> Playlist) -> SerieUpdate -> Client ()
updateTournament playlists (tournament, videos) = do
  let playlist = playlists tournament
  printPlaylist playlist
  liftIO.putStrLn $ "  " ++ count videos
  mapM_ (\(v, idx) -> insertVideo v idx playlist) videos
    where count []  = "no new video"
          count [_] = "1 new video"
          count xs  = show (length xs) ++ " new videos"
          printPlaylist = liftIO.putStrLn.playlistTitle

onlyNewVideos :: (Tournament -> Playlist) -> [Serie] -> Client [SerieUpdate]
onlyNewVideos ps = mapM (detectSerieUpdate ps)

sortSerie :: Serie -> Serie
sortSerie (t, vs) = (t, sortOn videoPublishDate vs)

detectSerieUpdate :: (Tournament -> Playlist) -> Serie -> Client SerieUpdate
detectSerieUpdate playlists (t, vs) = do
  update <- detectPlaylistUpdate (playlists t) vs
  return (t, update)

detectPlaylistUpdate :: Playlist -> [Video] -> Client [(Video, Position)]
detectPlaylistUpdate playlist vs = do
  oldVideos <- listPlaylist (playlistId playlist) 1000
  return $ findInsertPositions oldVideos vs

lastTournaments :: Int -> Client [Serie]
lastTournaments count = foundTournaments <$> getDataset count

foundTournaments :: Maybe Dataset -> [Serie]
foundTournaments  Nothing       = []
foundTournaments (Just dataset) = group tournamentsWithVideos
  where tournamentsWithVideos = mapMaybe extractTournament matchings
        matchings = computeMatchings dataset
        group ts = map ((fst . head) &&& map snd) $ groupBy (\p1 p2 -> fst p1 == fst p2) $ sortOn fst ts
        extractTournament (v, Perfect t) = Just (t, v)
        extractTournament  _             = Nothing

videosWithCasters :: Int -> Client ()
videosWithCasters count = do
  pId <- uploadsPlaylistId
  listPlaylist pId count >>= mapM_ (liftIO.videoWithCasters)

match :: Int -> Client ()
match count = getDataset count >>= liftIO.printMatchings

printMatchings :: Maybe Dataset -> IO ()
printMatchings (Just dataset) = mapM_ printMatching (computeMatchings dataset)
printMatchings  Nothing       = return ()

computeMatchings :: Dataset -> [(Video, Matching)]
computeMatchings (tournaments, videos) = decorate (matchTournaments tournaments) videos

printMatching :: (Video, Matching) -> IO ()
printMatching (video, Perfect tournament) = do
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

type Dataset = ([Tournament], Videos)

getDataset :: Int -> Client (Maybe Dataset)
getDataset count = do
  tournaments <- liftIO listTournaments
  pId <- uploadsPlaylistId
  playlistContent <- pure <$> listPlaylist pId count
  return $ (,) <$> tournaments <*> playlistContent
