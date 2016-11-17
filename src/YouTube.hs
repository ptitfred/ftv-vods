{-# LANGUAGE OverloadedStrings #-}

module YouTube
    (
      -- Features
      browseChannel
    , browseMyChannel
    , createPlaylist
    , createPlaylists
    , deletePlaylist
    , findChannel
    , insertVideo
    , listPlaylist
    , listVideos
      -- Reexport Models
    , Channel(..)
    , Endpoint(..)
    , Playlist(..)
    , Video(..)
    , Videos
    , YouTubeId
      -- Reexport Client utils
    , Client
    , runYouTubeClient
    ) where

import Helpers (hashURL)
import Model (Tournament(..))
import YouTube.Models
import GoogleAPIsClient

import Data.Foldable  (toList)
import Data.List      (find, intercalate)
import Data.Map  as M (empty, fromList, (!))

runYouTubeClient :: Client a -> IO a
runYouTubeClient = runClient youtube

youtube :: Endpoint
youtube = Endpoint "https://www.googleapis.com/youtube/v3" "https://www.googleapis.com/auth/youtube"

browseChannel :: YouTubeId -> Int -> Client [Playlist]
browseChannel cId count =
  unwrap <$> paginate (browseChannelHandler cId) count
    where unwrap (Playlists playlists) = playlists

browseMyChannel :: Int -> Client [Playlist]
browseMyChannel count = do
  needsUserCredentials
  myChannel <- channelId <$> findMyChannel
  browseChannel myChannel count

createPlaylist :: Tournament -> Client Playlist
createPlaylist t = do
  needsUserCredentials
  playlists <- browseMyChannel 1000
  findOrCreatePlaylist playlists t

createPlaylists :: [Tournament] -> Client (Tournament -> Playlist)
createPlaylists [] = return (M.empty M.!)
createPlaylists ts = do
  needsUserCredentials
  playlists <- browseMyChannel 1000
  findOrCreatePlaylists playlists ts

deletePlaylist :: YouTubeId -> Client Bool
deletePlaylist pId = do
  needsUserCredentials
  delete "/playlists" [ ("id", pId) ]

findChannel :: String -> Client Channel
findChannel name =
  firstChannel <$> get "/channels" parameters
    where parameters = [ ("part",       "id,contentDetails")
                       , ("forUsername", name              )
                       ]

insertVideo :: Video -> Int -> Playlist -> Client Success
insertVideo v pos pl = do
  needsUserCredentials
  post "/playlistItems" parameters body
    where parameters = [ ("part", "snippet") ]
          body = Just (PlaylistItem "" vId pId pos)
          vId = videoId v
          pId = playlistId pl

listPlaylist :: YouTubeId -> Int -> Client Videos
listPlaylist pId count = do
  needsUserCredentials
  ids <- map playlistItemVideoId . toList <$> paginate (listPlaylistHandler pId) count
  listVideos ids

type Videos = [Video]

listVideos :: [YouTubeId] -> Client Videos
listVideos vIds = toList <$> listVideosBatch (take 50 vIds) (drop 50 vIds)

listVideosBatch :: [YouTubeId] -> [YouTubeId] -> Client (Items Video)
listVideosBatch [] _ = return mempty
listVideosBatch ids otherIds = do
  let part       = "contentDetails,snippet"
      parameters = [ ("part", part               )
                   , ("id",   intercalate "," ids)
                   ]
  batch <- get "/videos" parameters
  mappend batch <$> listVideosBatch (take 50 otherIds) (drop 50 otherIds)

findMyChannel :: Client Channel
findMyChannel =
  firstChannel <$> get "/channels" parameters
    where parameters = [ ("part", "id,contentDetails"), ("mine", "true") ]

firstChannel :: Items Channel -> Channel
firstChannel (Items cs) = head cs

browseChannelHandler :: YouTubeId -> PageHandler Playlists
browseChannelHandler cId page =
  get "/playlists" (withPage page parameters)
    where part       = "contentDetails,snippet"
          parameters = [ ("part"      , part)
                       , ("channelId" , cId )
                       ]

listPlaylistHandler :: YouTubeId -> PageHandler PlaylistContent
listPlaylistHandler pId page =
  get "/playlistItems" (withPage page parameters)
    where part       = "contentDetails,snippet"
          parameters = [ ("part"      , part)
                       , ("playlistId", pId )
                       ]

findOrCreatePlaylists :: [Playlist] -> [Tournament] -> Client (Tournament -> Playlist)
findOrCreatePlaylists _ []  = return (\_ -> error "no tournament")
findOrCreatePlaylists ps ts = ((M.!) . M.fromList) <$> mapM (\t -> (,) t <$> findOrCreatePlaylist ps t) ts

findOrCreatePlaylist :: [Playlist] -> Tournament -> Client Playlist
findOrCreatePlaylist ps tournament = do
  let previous = find (samePlaylist playlist) ps
  case previous of
    Just found -> return found
    Nothing    -> post "/playlists" parameters body
      where parameters = [ ("part", "contentDetails,snippet,status") ]
            body = Just playlist
  where playlist = mkPlaylist tournament

samePlaylist :: Playlist -> Playlist -> Bool
samePlaylist p1 p2 = playlistTags p1 == playlistTags p2

mkPlaylist :: Tournament -> Playlist
mkPlaylist tournament = Playlist "" title description tags
  where title = tournamentName tournament
        description = tournamentURL tournament
        tag = hashURL $ tournamentURL tournament
        tags = Tags [tag]
