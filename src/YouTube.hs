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

import GoogleAPIsClient (Client, Endpoint(..), runClient)
import YouTube.Models
import YouTube.Services

runYouTubeClient :: Client a -> IO a
runYouTubeClient = runClient youtube
  where youtube = Endpoint "https://www.googleapis.com/youtube/v3" "https://www.googleapis.com/auth/youtube"
