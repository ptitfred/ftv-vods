{-# LANGUAGE OverloadedStrings #-}

module YouTube
    ( listPlaylist
    ) where

import Helpers
import Model
import YouTube.Client

import Data.Aeson
import Data.Time.Clock (UTCTime)

listPlaylist :: ApiKey -> YouTubeId -> Int -> IO PlaylistContent
listPlaylist key playlistId = paginate (listPlaylistHandler key playlistId)

listPlaylistHandler :: ApiKey -> YouTubeId -> Page -> IO (Response PlaylistContent)
listPlaylistHandler apiKey playlistId page = get (mkUrl url parameters)
  where Page token count = page
        url = "https://www.googleapis.com/youtube/v3/playlistItems"
        parameters = [ ("part"      , "contentDetails,snippet")
                     , ("maxResults", show count              )
                     , ("playlistId", playlistId              )
                     , ("key"       , apiKey                  )
                     , ("pageToken" , readToken token         )
                     ]

instance FromJSON VideoDetails where
  parseJSON (Object object) = do
    snippet        <- object .: "snippet"
    contentDetails <- object .: "contentDetails"
    mkVideoDetails <$> snippet .: "title"
                   <*> contentDetails .: "videoId"
                   <*> snippet .: "description"
                   <*> snippet .: "publishedAt"

mkVideoDetails :: String -> YouTubeId -> String -> UTCTime -> VideoDetails
mkVideoDetails title videoId description publishDate =
  VideoDetails title videoId description casters url publishDate
    where casters = extractCasters description
          url = "https://www.youtube.com/watch?v=" ++ videoId

instance FromJSON PlaylistContent where
  parseJSON (Object object) = PlaylistContent <$> object .: "items"
  parseJSON _               = return mempty
