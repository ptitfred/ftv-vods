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
    , casterPseudos
    , casters
    , isCaster
    , isPerfect
    , isPremier
    , isStandard
    , ofScore
    , ofTournament
    ) where

import Data.Time.Clock (UTCTime)

{- Some aliases --------------------------------------------------------------}
type ApiKey = String
type Name = String
type Score = Rational
type Scores = [Scoring]
type URL = String
type YoutubeId = String

{- Scoring -------------------------------------------------------------------}
type Scoring = (Tournament, Score)

ofTournament :: Scoring -> Tournament
ofTournament = fst

ofScore :: Scoring -> Score
ofScore = snd

{- TournamentType ------------------------------------------------------------}
data TournamentType = Premier | Standard deriving (Show)

{- Matching ------------------------------------------------------------------}
data Matching = Perfect Tournament | Approx Scores | NoMatch

isPerfect :: Matching -> Bool
isPerfect (Perfect _) = True
isPerfect _ = False

-- Tournament ----------------------------------------------------------------}
data Tournament = Tournament { tournamentName :: Name
                             , tournamentURL  :: URL
                             , tournamentType :: TournamentType
                             } deriving (Show)

isPremier :: Tournament -> Bool
isPremier (Tournament _ _ Premier) = True
isPremier _ = False

isStandard :: Tournament -> Bool
isStandard = not.isPremier

{- PlaylistContent -----------------------------------------------------------}
data PlaylistContent = PlaylistContent { videoDetails :: [VideoDetails] }

-- Implement Monoid to let concat queries
instance Monoid PlaylistContent where
  mempty        = PlaylistContent []
  mappend p1 p2 = PlaylistContent (videoDetails p1 ++ videoDetails p2)

{- VideoDetails --------------------------------------------------------------}
data VideoDetails = VideoDetails { videoTitle       :: String
                                 , videoId          :: YoutubeId
                                 , videoDescription :: String
                                 , videoCasters     :: [Caster]
                                 , videoURL         :: URL
                                 , videoPublishDate :: UTCTime
                                 }

{- Caster --------------------------------------------------------------------}
data Caster = Caster { casterPseudo     :: Name
                     , casterAliases    :: [Name]
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
