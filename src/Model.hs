{-# LANGUAGE OverloadedStrings #-}

module Model
    ( ApiKey
    , Name
    , PlaylistContent(..)
    , Tournament(..)
    , URL
    , VideoDetails(..)
    , YoutubeId
    ) where

import Data.Aeson
import Control.Applicative

type ApiKey = String
type YoutubeId = String

data VideoDetails = VideoDetails { videoTitle :: String, videoId :: YoutubeId } deriving (Show)
data PlaylistContent = PlaylistContent { videoDetails :: [VideoDetails] } deriving (Show)

type Name = String
type URL = String
data Tournament = Tournament { tournamentName :: Name, tournamentURL :: URL } deriving (Show)

instance FromJSON VideoDetails where
  parseJSON (Object object) = do
    snippet        <- object .: "snippet"
    contentDetails <- object .: "contentDetails"
    VideoDetails <$> snippet .: "title" <*> contentDetails .: "videoId"

instance FromJSON PlaylistContent where
  parseJSON (Object object) = PlaylistContent <$> object .: "items"
