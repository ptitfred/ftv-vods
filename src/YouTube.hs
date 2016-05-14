{-# LANGUAGE OverloadedStrings #-}

module YouTube
    ( Credentials
    , Playlist(..)
    , Video(..)
    , YouTubeId
    , listPlaylist
    ) where

import Helpers
import Model
import YouTube.Client

import Data.Aeson
import Data.Time.Clock (UTCTime)

type YouTubeId = String

data Playlist = Playlist { videos :: [Video] }

-- Implement Monoid to let concat queries
instance Monoid Playlist where
  mempty        = Playlist []
  mappend p1 p2 = Playlist (videos p1 ++ videos p2)

data Video = Video { videoTitle :: String
                   , videoId          :: YouTubeId
                   , videoDescription :: String
                   , videoCasters     :: [Caster]
                   , videoURL         :: URL
                   , videoPublishDate :: UTCTime
                   }

instance FromJSON Video where
  parseJSON (Object object) = do
    snippet        <- object .: "snippet"
    contentDetails <- object .: "contentDetails"
    mkVideoDetails <$> snippet .: "title"
                   <*> contentDetails .: "videoId"
                   <*> snippet .: "description"
                   <*> snippet .: "publishedAt"

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

listPlaylist :: YouTubeId -> Int -> IO Playlist
listPlaylist playlistId count = do
  credentials <- getCredentials
  paginate (listPlaylistHandler credentials playlistId) count

listPlaylistHandler :: Credentials -> YouTubeId -> Page -> IO (Response Playlist)
listPlaylistHandler credentials playlistId page = get (mkUrl url parameters)
  where Page token count = page
        url = "https://www.googleapis.com/youtube/v3/playlistItems"
        parameters = [ ("part"      , "contentDetails,snippet")
                     , ("maxResults", show count              )
                     , ("pageToken" , show token              )
                     , ("playlistId", playlistId              )
                     , ("key"       , apiKey credentials      )
                     ]

instance FromJSON Playlist where
  parseJSON (Object object) = Playlist <$> object .: "items"
