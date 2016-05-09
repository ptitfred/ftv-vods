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

type ApiKey = String
type YoutubeId = String
type Score = Rational

data VideoDetails = VideoDetails { videoTitle :: String, videoId :: YoutubeId } deriving (Show)
data PlaylistContent = PlaylistContent { videoDetails :: [VideoDetails] } deriving (Show)

type Name = String
type URL = String
data Tournament = Tournament { tournamentName :: Name, tournamentURL :: URL, tournamentType :: TournamentType } deriving (Show)

data TournamentType = Premier | Standard deriving (Show)

data Matching = Perfect Tournament | Approx Scores | NoMatch

isPremier :: Tournament -> Bool
isPremier (Tournament _ _ Premier) = True
isPremier _ = False

