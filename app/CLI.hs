module Main where

import Liquipedia
import Matcher
import Model
import YouTube

import Data.List (intercalate, groupBy, sortOn, (\\))
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
  let items = [(v, idx, playlists t) | (t, vs) <- tournamentsWithNewVideos, (v, idx) <- vs]
  mapM_ (\(v, idx, p) -> insertVideo v idx p) items
  mapM_ (liftIO.printPlaylist) (map playlists tournaments)
    where printPlaylist = putStrLn.playlistTitle

onlyNewVideos :: (Tournament -> Playlist) -> [Serie] -> Client [SerieUpdate]
onlyNewVideos ps = mapM (detectPlaylistUpdate ps . sortSerie)

type Serie = (Tournament, [Video])
type SerieUpdate = (Tournament, [(Video, Int)])

sortSerie :: Serie -> Serie
sortSerie (t, vs) = (t, sortOn videoPublishDate vs)

detectPlaylistUpdate :: (Tournament -> Playlist) -> Serie -> Client SerieUpdate
detectPlaylistUpdate playlists (tournament, vs) = do
  oldVideos <- listPlaylist (playlistId $ playlists tournament) 1000
  let newVideos = reverse $ vs \\ oldVideos
  let newPositions = decorate (findInsertPosition oldVideos) newVideos
  return (tournament, newPositions)

decorate :: (a -> b) -> [a] -> [(a, b)]
decorate f = map (\x -> (x, f x))

findInsertPosition :: [Video] -> Video -> Int
findInsertPosition [] _ = 0
findInsertPosition vs v = snd $ head $ dropWhile (\(v', _) -> v' `before` v) $ withIndex vs
  where v1 `before` v2 = videoPublishDate v1 < videoPublishDate v2

withIndex :: [a] -> [(a, Int)]
withIndex xs = zip xs [0..]

lastTournaments :: Int -> Client [Serie]
lastTournaments count = foundTournaments <$> getDataset count

foundTournaments :: Maybe Dataset -> [Serie]
foundTournaments  Nothing       = []
foundTournaments (Just dataset) = group tournamentsWithVideos
  where tournamentsWithVideos = catMaybes $ map extractTournament matchings
        matchings = computeMatchings dataset
        group ts = map (\tvs -> (fst . head $ tvs, map snd tvs)) $ groupBy (\p1 p2 -> fst p1 == fst p2) $ sortOn fst ts
        extractTournament (v, (Perfect t)) = Just (t, v)
        extractTournament  _               = Nothing

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

type Dataset = ([Tournament], Videos)

getDataset :: Int -> Client (Maybe Dataset)
getDataset count = do
  tournaments <- liftIO listTournaments
  pId <- uploadsPlaylistId
  playlistContent <- pure <$> listPlaylist pId count
  return $ (,) <$> tournaments <*> playlistContent
