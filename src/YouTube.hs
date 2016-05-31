{-# LANGUAGE OverloadedStrings #-}

module YouTube
    ( Credentials
    , Playlist(..)
    , PlaylistContent(..)
    , Video(..)
    , YouTubeId
    , createPlaylist
    , createPlaylists
    , deletePlaylist
    , listPlaylist
    , browseChannel
    ) where

import Helpers
import Model
import YouTube.Commons
import YouTube.Client

import Crypto.Hash      (Digest, SHA1, hash)
import Data.Aeson       hiding (Result)
import Data.Aeson.Types (typeMismatch)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 as C (pack)
import Data.Time.Clock  (UTCTime)

type YouTubeId = String

data Playlist = Playlist { playlistId :: YouTubeId
                         , playlistTitle :: String
                         , playlistDescription :: String
                         , playlistTags :: Tags
                         } deriving (Show)

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

browseChannel :: UserCredentials -> YouTubeId -> Int -> IO [Playlist]
browseChannel userCredentials channelId count = do
  credentials <- getCredentials
  unwrap <$> paginate (browseChannelHandler credentials userCredentials channelId) count
  where unwrap (Playlists playlists) = playlists

browseChannelHandler :: Credentials -> UserCredentials -> YouTubeId -> PageHandler Playlists
browseChannelHandler credentials userCredentials channelId page =
  get userCredentials {- NoCredentials -} (mkUrl url parameters)
    where Page token count = page
          url = "https://www.googleapis.com/youtube/v3/playlists"
          part = "contentDetails,snippet"
          parameters = [ ("part"      , part               )
                       , ("maxResults", show count         )
                       , ("pageToken" , show token         )
                       , ("channelId" , channelId          )
                       , ("key"       , apiKey credentials )
                       ]

listPlaylist :: YouTubeId -> Int -> IO PlaylistContent
listPlaylist pId count = do
  credentials <- getCredentials
  paginate (listPlaylistHandler credentials pId) count

listPlaylistHandler :: Credentials -> YouTubeId -> PageHandler PlaylistContent
listPlaylistHandler credentials pId page = get NoCredentials (mkUrl url parameters)
  where Page token count = page
        url = "https://www.googleapis.com/youtube/v3/playlistItems"
        part = "contentDetails,snippet"
        parameters = [ ("part"      , part              )
                     , ("maxResults", show count        )
                     , ("pageToken" , show token        )
                     , ("playlistId", pId               )
                     , ("key"       , apiKey credentials)
                     ]

instance ToJSON Playlist where
  toJSON playlist =
    object
      [ "snippet" .=
         object [ "title"       .= playlistTitle playlist
                , "description" .= playlistDescription playlist
                , "tags"        .= playlistTags playlist
                ]
      ]

newtype Tags = Tags [String] deriving (Show)

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

instance FromJSON PlaylistContent where
  parseJSON (Object o) = PlaylistContent <$> o .: "items"
  parseJSON invalid = typeMismatch "PlaylistContent" invalid

newtype Playlists = Playlists [Playlist] deriving (Show)

instance Monoid Playlists where
  mempty  = Playlists []
  mappend (Playlists ps1) (Playlists ps2) = Playlists $ ps1 ++ ps2

instance FromJSON Playlists where
  parseJSON (Object o) = Playlists <$> o .: "items"
  parseJSON invalid = typeMismatch "[Playlist]" invalid

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
          tag = hashURL $ tournamentURL tournament
          tags = Tags [tag]
          playlist = Playlist "" title description tags
          body = Just playlist

hashURL :: URL -> String
hashURL = show . sha1 . C.pack

sha1 :: ByteString -> Digest SHA1
sha1 = hash

createPlaylists :: [Tournament] -> IO [Playlist]
createPlaylists [] = return []
createPlaylists ts = getUserCredentials >>= createPlaylists' ts

createPlaylists' :: [Tournament] -> UserCredentials -> IO [Playlist]
createPlaylists' [] _ = return []
createPlaylists' ts c = mapM (\t -> createPlaylist' t c) ts

deletePlaylist :: YouTubeId -> IO Bool
deletePlaylist pId = getUserCredentials >>= flip delete url
  where url = mkUrl "DELETE https://www.googleapis.com/youtube/v3/playlists" parameters
        parameters = [ ("id", pId) ]
