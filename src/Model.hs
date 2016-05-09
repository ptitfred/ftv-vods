module Model
    ( ApiKey
    , Matching(..)
    , Name
    , PlaylistContent(..)
    , Score
    , Scores
    , Scoring
    , Tournament(..)
    , TournamentType(..)
    , URL
    , VideoDetails(..)
    , YoutubeId
    , isPerfect
    , isPremier
    , ofTournament
    , ofScore
    ) where

type ApiKey = String
type YoutubeId = String
type Score = Rational
type Scoring = (Tournament, Score)
type Scores = [Scoring]

data VideoDetails = VideoDetails { videoTitle :: String, videoId :: YoutubeId, videoDescription :: String } deriving (Show)
data PlaylistContent = PlaylistContent { videoDetails :: [VideoDetails] } deriving (Show)

type Name = String
type URL = String
data Tournament = Tournament { tournamentName :: Name, tournamentURL :: URL, tournamentType :: TournamentType } deriving (Show)

data TournamentType = Premier | Standard deriving (Show)

data Matching = Perfect Tournament | Approx Scores | NoMatch

isPremier :: Tournament -> Bool
isPremier (Tournament _ _ Premier) = True
isPremier _ = False

isPerfect :: Matching -> Bool
isPerfect (Perfect _) = True
isPerfect _ = False

ofTournament :: Scoring -> Tournament
ofTournament = fst

ofScore :: Scoring -> Score
ofScore = snd
