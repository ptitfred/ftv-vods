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
      -- Models
    , Channel(..)
    , Credentials
    , Playlist(..)
    , PlaylistContent(..)
    , Video(..)
    , YouTubeId
      -- Reexport Client utils
    , Client
    , runClient
    , liftIO
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
import Data.Map              as M (empty, fromList, (!))
import Data.Time.Clock            (UTCTime)

browseMyChannel :: Int -> Client [Playlist]
browseMyChannel count = do
  myChannel <- channelId <$> findMyChannel
  browseChannel myChannel count

newtype Items a = Items [a]

instance FromJSON a => FromJSON (Items a) where
  parseJSON (Object o) = Items <$> o .: "items"
  parseJSON invalid = typeMismatch "items" invalid

findChannel :: String -> Client Channel
findChannel name =
  firstChannel <$> get "/channels" parameters
    where parameters = [ ("part",       "id,contentDetails" )
                       , ("forUsername", name)
                       ]

findMyChannel :: Client Channel
findMyChannel = do
  needsUserCredentials
  firstChannel <$> get "/channels" parameters
    where parameters = [ ("part", "id,contentDetails"), ("mine", "true") ]

firstChannel :: Items Channel -> Channel
firstChannel (Items cs) = head cs

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

data PlaylistContent = PlaylistContent { videos :: [Video] }

-- Implement Monoid to let concat queries
instance Monoid PlaylistContent where
  mempty        = PlaylistContent []
  mappend p1 p2 = PlaylistContent (videos p1 ++ videos p2)

data Video = Video { videoTitle       :: String
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

browseChannel :: YouTubeId -> Int -> Client [Playlist]
browseChannel cId count = do
  unwrap <$> paginate (browseChannelHandler cId) count
    where unwrap (Playlists playlists) = playlists

browseChannelHandler :: YouTubeId -> PageHandler Playlists
browseChannelHandler cId page =
  get "/playlists" (withPage page parameters)
    where part       = "contentDetails,snippet"
          parameters = [ ("part"      , part)
                       , ("channelId" , cId )
                       ]

listPlaylist :: YouTubeId -> Int -> Client PlaylistContent
listPlaylist pId count = do
  needsUserCredentials
  paginate (listPlaylistHandler pId) count

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

createPlaylist :: Tournament -> Client Playlist
createPlaylist t = do
  playlists <- browseMyChannel 1000
  createPlaylist' playlists t

createPlaylists :: [Tournament] -> Client (Tournament -> Playlist)
createPlaylists [] = return (M.empty M.!)
createPlaylists ts = do
  playlists <- browseMyChannel 1000
  createPlaylists' playlists ts

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

insertVideo :: YouTubeId -> YouTubeId -> Client Success
insertVideo vId pId = do
  needsUserCredentials
  post "/playlistItems" parameters body
    where parameters = [ ("part", "snippet") ]
          body = Just (PlaylistItem vId pId)

data PlaylistItem = PlaylistItem { playlistItemVideoId :: YouTubeId
                                 , playlistItemPlaylistId :: YouTubeId
                                 }

instance ToJSON PlaylistItem where
  toJSON item =
    object [ "snippet" .=
               object [ "playlistId" .= playlistItemPlaylistId item
                      , "resourceId" .=
                         object [ "kind"    .= kind
                                , "videoId" .= playlistItemVideoId item
                                ]
                      ]
           ]
      where kind = "youtube#video" :: String

createPlaylists' :: [Playlist] -> [Tournament] -> Client (Tournament -> Playlist)
createPlaylists' _ []  = return (\_ -> error "no tournament")
createPlaylists' ps ts = ((M.!) . M.fromList) <$> mapM (\t -> (,) t <$> createPlaylist' ps t) ts

createPlaylist' :: [Playlist] -> Tournament -> Client Playlist
createPlaylist' ps tournament = do
  needsUserCredentials
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

hashURL :: URL -> String
hashURL = show . sha1 . C.pack

sha1 :: ByteString -> Digest SHA1
sha1 = hash

deletePlaylist :: YouTubeId -> Client Bool
deletePlaylist pId = do
  needsUserCredentials
  delete "/playlists" [ ("id", pId) ]
