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
      -- Reexport Models
    , Channel(..)
    , Playlist(..)
    , PlaylistContent(..)
    , Video(..)
    , YouTubeId
      -- Reexport Client utils
    , Client
    , runClient
    , liftIO
    ) where

import Helpers (hashURL)
import Model (Tournament(..))
import YouTube.Commons
import YouTube.Client
import YouTube.Models

import Data.List      (find)
import Data.Map  as M (empty, fromList, (!))

browseChannel :: YouTubeId -> Int -> Client [Playlist]
browseChannel cId count = do
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
  createPlaylist' playlists t

createPlaylists :: [Tournament] -> Client (Tournament -> Playlist)
createPlaylists [] = return (M.empty M.!)
createPlaylists ts = do
  needsUserCredentials
  playlists <- browseMyChannel 1000
  createPlaylists' playlists ts

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

insertVideo :: YouTubeId -> YouTubeId -> Client Success
insertVideo vId pId = do
  needsUserCredentials
  post "/playlistItems" parameters body
    where parameters = [ ("part", "snippet") ]
          body = Just (PlaylistItem vId pId)

listPlaylist :: YouTubeId -> Int -> Client PlaylistContent
listPlaylist pId count = do
  needsUserCredentials
  paginate (listPlaylistHandler pId) count

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
listPlaylistHandler pId page = do
  get "/playlistItems" (withPage page parameters)
    where part       = "contentDetails,snippet"
          parameters = [ ("part"      , part)
                       , ("playlistId", pId )
                       ]

withPage :: Page -> Parameters -> Parameters
withPage (Page token count) parameters =
  ("pageToken" , show token) : ("maxResults", show count) : parameters

createPlaylists' :: [Playlist] -> [Tournament] -> Client (Tournament -> Playlist)
createPlaylists' _ []  = return (\_ -> error "no tournament")
createPlaylists' ps ts = ((M.!) . M.fromList) <$> mapM (\t -> (,) t <$> createPlaylist' ps t) ts

createPlaylist' :: [Playlist] -> Tournament -> Client Playlist
createPlaylist' ps tournament = do
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
