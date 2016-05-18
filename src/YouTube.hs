{-# LANGUAGE OverloadedStrings #-}

module YouTube
    ( Credentials
    , Playlist(..)
    , PlaylistContent(..)
    , Video(..)
    , YouTubeId
    , createPlaylist
    , createPlaylists
    , listPlaylist
    ) where

import Helpers
import Model
import YouTube.Commons
import YouTube.Client

import Data.Aeson       hiding (Result)
import Data.Aeson.Types (typeMismatch)
import Data.Time.Clock  (UTCTime)

type YouTubeId = String

data Playlist = Playlist { playlistTitle :: String, playlistDescription :: String }

data PlaylistContent = PlaylistContent { videos :: [Video] }

-- Implement Monoid to let concat queries
instance Monoid PlaylistContent where
  mempty        = PlaylistContent []
  mappend p1 p2 = PlaylistContent (videos p1 ++ videos p2)

data Video = Video { videoTitle :: String
                   , videoId          :: YouTubeId
                   , videoDescription :: String
                   , videoCasters     :: [Caster]
                   , videoURL         :: URL
                   , videoPublishDate :: UTCTime
                   }

instance FromJSON Video where
  parseJSON (Object o) = do
    snippet        <- o .: "snippet"
    contentDetails <- o .: "contentDetails"
    mkVideoDetails <$> snippet .: "title"
                   <*> contentDetails .: "videoId"
                   <*> snippet .: "description"
                   <*> snippet .: "publishedAt"
  parseJSON invalid = typeMismatch "Video" invalid

mainCasters :: [Caster]
mainCasters = [ Caster "LuCiqNo"   []               Nothing
              , Caster "Hugo"      []               Nothing
              , Caster "v0ja"      ["voja", "Voja"] Nothing
              , Caster "YouYou"    ["Youyou"]       Nothing
              , Caster "Shiba"     []               Nothing
              , Caster "Gourouf"   ["MrGourouf"]    Nothing
              , Caster "7ckingMad" ["7uckingMad"]   Nothing
              , Caster "Namax"     []               Nothing
              , Caster "Darwyn"    []               Nothing
              ]

mkVideoDetails :: String -> YouTubeId -> String -> UTCTime -> Video
mkVideoDetails title vid description publishDate =
  Video title vid description casters url publishDate
    where casters = extractCasters mainCasters description
          url = "https://www.youtube.com/watch?v=" ++ vid

listPlaylist :: YouTubeId -> Int -> IO PlaylistContent
listPlaylist playlistId count = do
  credentials <- getCredentials
  paginate (listPlaylistHandler credentials playlistId) count

listPlaylistHandler :: Credentials -> YouTubeId -> Page -> IO (Result PlaylistContent)
listPlaylistHandler credentials playlistId page = get NoCredentials (mkUrl url parameters)
  where Page token count = page
        url = "https://www.googleapis.com/youtube/v3/playlistItems"
        parameters = [ ("part"      , "contentDetails,snippet")
                     , ("maxResults", show count              )
                     , ("pageToken" , show token              )
                     , ("playlistId", playlistId              )
                     , ("key"       , apiKey credentials      )
                     ]

instance ToJSON Playlist where
  toJSON playlist = object [
                      "snippet" .= object [
                        "title" .= playlistTitle playlist
                      , "description" .= playlistDescription playlist
                      ]
                    ]

instance FromJSON Playlist where
  parseJSON (Object o) = do
    snippet <- o .: "snippet"
    Playlist <$> snippet .: "title" <*> snippet .: "description"
  parseJSON invalid = typeMismatch "Playlist" invalid

instance FromJSON PlaylistContent where
  parseJSON (Object o) = PlaylistContent <$> o .: "items"
  parseJSON invalid = typeMismatch "PlaylistContent" invalid

createPlaylist :: Tournament -> IO Playlist
createPlaylist t = getUserCredentials >>= createPlaylist' t

createPlaylist' :: Tournament -> UserCredentials -> IO Playlist
createPlaylist' tournament creds = do
  post body creds (mkUrl url parameters)
    where url = "POST https://www.googleapis.com/youtube/v3/playlists"
          parameters = [ ("part", "contentDetails,snippet")
                       ]
          title = tournamentName tournament
          description = tournamentURL tournament
          body = Just $ Playlist title description

createPlaylists :: [Tournament] -> IO [Playlist]
createPlaylists [] = return []
createPlaylists ts = getUserCredentials >>= createPlaylists' ts

createPlaylists' :: [Tournament] -> UserCredentials -> IO [Playlist]
createPlaylists' [] _ = return []
createPlaylists' ts c = mapM (\t -> createPlaylist' t c) ts
