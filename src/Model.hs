module Model
    ( ApiKey
    , Caster(..)
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
    , isCaster
    , isPerfect
    , isPremier
    , ofTournament
    , ofScore
    , casters
    , casterPseudos
    ) where

type ApiKey = String
type YoutubeId = String
type Score = Rational
type Scoring = (Tournament, Score)
type Scores = [Scoring]

data VideoDetails = VideoDetails { videoTitle :: String
                                 , videoId :: YoutubeId
                                 , videoDescription :: String
                                 , videoCasters :: [Caster]
                                 , videoURL :: URL
                                 } deriving (Show)
data PlaylistContent = PlaylistContent { videoDetails :: [VideoDetails] } deriving (Show)

instance Monoid PlaylistContent where
  mempty        = PlaylistContent []
  mappend p1 p2 = PlaylistContent (videoDetails p1 ++ videoDetails p2)

type Name = String
type URL = String
data Tournament = Tournament { tournamentName :: Name, tournamentURL :: URL, tournamentType :: TournamentType } deriving (Show)

data TournamentType = Premier | Standard deriving (Show)

data Matching = Perfect Tournament | Approx Scores | NoMatch

data Caster = Caster { casterPseudo :: Name
                     , casterAliases :: [Name]
                     , casterPictureURL :: Maybe URL
                     } deriving (Show)

casterPseudos :: Caster -> [Name]
casterPseudos caster = casterPseudo caster : casterAliases caster

isCaster :: Name -> Caster -> Bool
isCaster name caster = name `elem` casterPseudos caster

casters :: [Caster]
casters = [ Caster "LuCiqNo"   []               Nothing
          , Caster "Hugo"      []               Nothing
          , Caster "v0ja"      ["voja", "Voja"] Nothing
          , Caster "YouYou"    ["Youyou"]       Nothing
          , Caster "Shiba"     []               Nothing
          , Caster "Gourouf"   ["MrGourouf"]    Nothing
          , Caster "7ckingMad" ["7uckingMad"]   Nothing
          , Caster "Namax"     []               Nothing
          , Caster "Darwyn"    []               Nothing
          ]

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
