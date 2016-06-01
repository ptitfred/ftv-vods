{-# LANGUAGE OverloadedStrings #-}

module YouTube
    ( Channel(..)
    , Credentials
    , Playlist(..)
    , PlaylistContent(..)
    , Video(..)
    , YouTubeId
    , createPlaylist
    , createPlaylists
    , deletePlaylist
    , findChannel
    , listPlaylist
    , browseChannel
    , browseMyChannel
    , insertVideo
    ) where

import Helpers
import Model
import YouTube.Commons
import YouTube.Client

import Crypto.Hash                (Digest, SHA1, hash)
import Data.Aeson                 hiding (Result, Success, Error)
import Data.Aeson.Types           (typeMismatch)
import Data.ByteString            (ByteString)
import Data.ByteString.Char8 as C (pack)
import Data.List                  (find)
import Data.Time.Clock            (UTCTime)

browseMyChannel :: Int -> IO [Playlist]
browseMyChannel count = do
  userCredentials <- getUserCredentials
  myChannel <- findMyChannel
  browseChannel userCredentials (channelId myChannel) count

newtype Items a = Items [a]

instance FromJSON a => FromJSON (Items a) where
  parseJSON (Object o) = Items <$> o .: "items"
  parseJSON invalid = typeMismatch "items" invalid

findChannel :: String -> IO Channel
findChannel name = do
  key <- apiKey <$> getCredentials
  let parameters = [ ("part",       "id,contentDetails" )
                   , ("forUsername", name)
                   , ("key",         key )
                   ]
  let url = mkUrl "GET https://www.googleapis.com/youtube/v3/channels" parameters
  firstChannel <$> get NoCredentials url

findMyChannel :: IO Channel
findMyChannel = do
  userCredentials <- getUserCredentials
  firstChannel <$> get userCredentials url
    where url = mkUrl "GET https://www.googleapis.com/youtube/v3/channels" parameters
          parameters = [ ("part", "id,contentDetails"), ("mine", "true") ]

firstChannel :: Items Channel -> Channel
firstChannel (Items cs) = head cs

data Channel = Channel { channelId :: YouTubeId
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
browseChannel userCredentials cId count = do
  credentials <- getCredentials
  unwrap <$> paginate (browseChannelHandler credentials userCredentials cId) count
  where unwrap (Playlists playlists) = playlists

browseChannelHandler :: Credentials -> UserCredentials -> YouTubeId -> PageHandler Playlists
browseChannelHandler credentials userCredentials cId page =
  get userCredentials url
    where Page token count = page
          url = mkUrl "https://www.googleapis.com/youtube/v3/playlists" parameters
          part = "contentDetails,snippet"
          parameters = [ ("part"      , part               )
                       , ("maxResults", show count         )
                       , ("pageToken" , show token         )
                       , ("channelId" , cId                )
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
createPlaylist t = do
  creds <- getUserCredentials
  playlists <- browseMyChannel 1000
  createPlaylist' playlists t creds

createPlaylists :: [Tournament] -> IO [Playlist]
createPlaylists [] = return []
createPlaylists ts = do
  creds <- getUserCredentials
  playlists <- browseMyChannel 1000
  createPlaylists' playlists ts creds

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

insertVideo :: YouTubeId -> YouTubeId -> IO Success
insertVideo vId pId = do
  creds <- getUserCredentials
  post body creds url
    where body = Just $ PlaylistItem vId pId
          url = mkUrl "POST https://www.googleapis.com/youtube/v3/playlistItems" parameters
          parameters = [ ("part", "snippet") ]

data PlaylistItem = PlaylistItem { playlistItemVideoId :: YouTubeId
                                 , playlistItemPlaylistId :: YouTubeId
                                 }

instance ToJSON PlaylistItem where
  toJSON item =
    object [ "snippet" .=
               object [ "playlistId" .= playlistItemPlaylistId item
                      , "resourceId" .=
                         object [ "kind"    .= ("youtube#video" :: String)
                                , "videoId" .= playlistItemVideoId item
                                ]
                      ]
           ]

createPlaylists' :: [Playlist] -> [Tournament] -> UserCredentials -> IO [Playlist]
createPlaylists' _ [] _ = return []
createPlaylists' ps ts c = mapM (\t -> createPlaylist' ps t c) ts

createPlaylist' :: [Playlist] -> Tournament -> UserCredentials -> IO Playlist
createPlaylist' ps tournament creds = do
  let previous = find (samePlaylist playlist) ps
  case previous of
    Just found -> return found
    Nothing    -> post body creds url
  where url = mkUrl "POST https://www.googleapis.com/youtube/v3/playlists" parameters
        parameters = [ ("part", "contentDetails,snippet,status") ]
        playlist = mkPlaylist tournament
        body = Just playlist

samePlaylist :: Playlist -> Playlist -> Bool
samePlaylist p1 p2 = playlistTags p1 == playlistTags p2

mkPlaylist :: Tournament -> Playlist
mkPlaylist tournament = Playlist "" title description tags
  where title = tournamentName tournament
        description = tournamentURL tournament
        tag = hashURL $ tournamentURL tournament
        tags = Tags [tag]

hashURL :: URL -> String
hashURL = show . sha1 . C.pack

sha1 :: ByteString -> Digest SHA1
sha1 = hash

deletePlaylist :: YouTubeId -> IO Bool
deletePlaylist pId = getUserCredentials >>= flip delete url
  where url = mkUrl "DELETE https://www.googleapis.com/youtube/v3/playlists" parameters
        parameters = [ ("id", pId) ]
