{-# LANGUAGE OverloadedStrings #-}

module Model
    ( ApiKey
    , Name
    , PlaylistContent(..)
    , Score
    , Tournament(..)
    , TournamentType(..)
    , URL
    , VideoDetails(..)
    , YoutubeId
    , isPremier
    ) where

import Data.Aeson
import Control.Applicative

type ApiKey = String
type YoutubeId = String
type Score = Rational

data VideoDetails = VideoDetails { videoTitle :: String, videoId :: YoutubeId } deriving (Show)
data PlaylistContent = PlaylistContent { videoDetails :: [VideoDetails] } deriving (Show)

type Name = String
type URL = String
data Tournament = Tournament { tournamentName :: Name, tournamentURL :: URL, tournamentType :: TournamentType } deriving (Show)

data TournamentType = Premier | Standard deriving (Show)

isPremier :: Tournament -> Bool
isPremier (Tournament _ _ Premier) = True
isPremier _ = False

instance FromJSON VideoDetails where
  parseJSON (Object object) = do
    snippet        <- object .: "snippet"
    contentDetails <- object .: "contentDetails"
    VideoDetails <$> snippet .: "title" <*> contentDetails .: "videoId"

instance FromJSON PlaylistContent where
  parseJSON (Object object) = PlaylistContent <$> object .: "items"
