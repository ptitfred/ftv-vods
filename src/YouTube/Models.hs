{-# LANGUAGE OverloadedStrings #-}

module YouTube.Models
    ( Channel(..)
    , Error(..)
    , Items(..)
    , Playlist(..)
    , Playlists(..)
    , PlaylistItem(..)
    , PlaylistContent
    , Success(..)
    , Tags(..)
    , Video(..)
    , Videos
    , YouTubeId
    ) where

import Helpers
import Model

import Data.Aeson       hiding (Result, Success, Error)
import Data.Aeson.Types (typeMismatch)
import Data.Time.Clock  (UTCTime)

data Channel = Channel { channelId             :: YouTubeId
                       , channelUploadPlaylist :: YouTubeId
                       } deriving (Show)

instance FromJSON Channel where
  parseJSON (Object o) = do
    contentDetails   <- o .: "contentDetails"
    relatedPlaylists <- contentDetails .: "relatedPlaylists"
    Channel <$> o .: "id"
            <*> relatedPlaylists .: "uploads"
  parseJSON invalid = typeMismatch "Channel" invalid

type YouTubeId = String

data Playlist = Playlist { playlistId          :: YouTubeId
                         , playlistTitle       :: String
                         , playlistDescription :: String
                         , playlistTags        :: Tags
                         } deriving (Show)

type PlaylistContent = Items PlaylistItem

data Video = Video { videoTitle       :: String
                   , videoId          :: YouTubeId
                   , videoDescription :: String
                   , videoCasters     :: [Caster]
                   , videoURL         :: URL
                   , videoPublishDate :: UTCTime
                   } deriving (Show)

instance FromJSON Video where
  parseJSON (Object o) = do
    snippet        <- o .: "snippet"
    mkVideoDetails <$> snippet .: "title"
                   <*> o       .: "id"
                   <*> snippet .: "description"
                   <*> snippet .: "publishedAt"
  parseJSON invalid = typeMismatch "Video" invalid

instance Eq Video where
  v1 == v2 = videoId v1 == videoId v2

type Videos = [Video]

mkVideoDetails :: String -> YouTubeId -> String -> UTCTime -> Video
mkVideoDetails title vid description = Video title vid description casters url
  where casters = extractCasters mainCasters description
        url = "https://www.youtube.com/watch?v=" ++ vid

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

instance ToJSON Playlist where
  toJSON playlist =
    object
      [ "snippet" .=
         object [ "title"       .= playlistTitle playlist
                , "description" .= playlistDescription playlist
                , "tags"        .= playlistTags playlist
                ]
      , "status" .=
         object [ "privacyStatus" .= ("unlisted" :: String)
                ]
      ]

newtype Tags = Tags [String] deriving (Show, Eq)

instance ToJSON Tags where
  toJSON (Tags tags) = toJSON tags

instance FromJSON Tags where
  parseJSON o = Tags <$> parseJSON o

instance FromJSON Playlist where
  parseJSON (Object o) = do
    snippet  <- o .: "snippet"
    Playlist <$> o .: "id"
             <*> snippet .: "title"
             <*> snippet .: "description"
             <*> snippet .:? "tags" .!= Tags []
  parseJSON invalid = typeMismatch "Playlist" invalid

newtype Playlists = Playlists [Playlist] deriving (Show)

instance Monoid Playlists where
  mempty  = Playlists []
  mappend (Playlists ps1) (Playlists ps2) = Playlists $ ps1 ++ ps2

instance FromJSON Playlists where
  parseJSON (Object o) = Playlists <$> o .: "items"
  parseJSON invalid = typeMismatch "[Playlist]" invalid

data PlaylistItem = PlaylistItem { playlistItemId         :: YouTubeId
                                 , playlistItemVideoId    :: YouTubeId
                                 , playlistItemPlaylistId :: YouTubeId
                                 , playlistItemPosition   :: Int
                                 } deriving (Show)

instance FromJSON PlaylistItem where
  parseJSON (Object o) = do
    snippet    <- o .: "snippet"
    resourceId <- snippet .: "resourceId"
    PlaylistItem <$> o          .: "id"
                 <*> resourceId .: "videoId"
                 <*> snippet    .: "playlistId"
                 <*> snippet    .: "position"
  parseJSON invalid = typeMismatch "PlaylistItem" invalid

instance ToJSON PlaylistItem where
  toJSON item =
    object [ "snippet" .=
               object [ "playlistId" .= playlistItemPlaylistId item
                      , "resourceId" .=
                         object [ "kind"    .= kind
                                , "videoId" .= playlistItemVideoId item
                                ]
                      , "position" .= playlistItemPosition item
                      ]
           ]
      where kind = "youtube#video" :: String

newtype Items a = Items [a]

instance FromJSON a => FromJSON (Items a) where
  parseJSON (Object o) = Items <$> o .: "items"
  parseJSON invalid = typeMismatch "items" invalid

instance Monoid (Items a) where
  mempty = Items []
  mappend (Items items1) (Items items2) = Items $ items1 ++ items2

instance Show a => Show (Items a) where
  show (Items xs) = show xs

instance Functor Items where
  fmap f (Items xs) = Items $ fmap f xs

instance Foldable Items where
  foldMap f (Items xs) = foldMap f xs
  foldr f z (Items xs) = foldr f z xs

newtype Success = Success Bool deriving (Show)

instance FromJSON Success where
  parseJSON (Object o) = do
    e <- o .:? "error" .!= Error 200
    return $ Success $ errorCode e == 200
  parseJSON _ = return $ Success False

data Error = Error { errorCode :: Int } deriving (Show)

instance FromJSON Error where
  parseJSON (Object o) = Error <$> o .: "code"
  parseJSON invalid = typeMismatch "Error" invalid
