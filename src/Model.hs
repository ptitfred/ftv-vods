module Model
    ( Caster(..)
    , Matching(..)
    , Name
    , Score
    , Scores
    , Scoring
    , Tournament(..)
    , TournamentType(..)
    , URL
    , casterPseudos
    , isCaster
    , isPerfect
    , isPremier
    , isStandard
    , ofScore
    , ofTournament
    ) where

{- Some aliases --------------------------------------------------------------}
type Name = String
type Score = Rational
type Scores = [Scoring]
type URL = String

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

{- Tournament ----------------------------------------------------------------}
data Tournament = Tournament { tournamentName :: Name
                             , tournamentURL  :: URL
                             , tournamentType :: TournamentType
                             } deriving (Show)

isPremier :: Tournament -> Bool
isPremier (Tournament _ _ Premier) = True
isPremier _ = False

isStandard :: Tournament -> Bool
isStandard = not.isPremier

{- Caster --------------------------------------------------------------------}
data Caster = Caster { casterPseudo     :: Name
                     , casterAliases    :: [Name]
                     , casterPictureURL :: Maybe URL
                     } deriving (Show)

casterPseudos :: Caster -> [Name]
casterPseudos caster = casterPseudo caster : casterAliases caster

isCaster :: Name -> Caster -> Bool
isCaster name caster = name `elem` casterPseudos caster
