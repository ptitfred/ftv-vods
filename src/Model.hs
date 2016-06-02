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
data TournamentType = Premier | Standard deriving (Show, Eq)

{- Matching ------------------------------------------------------------------}
data Matching = Perfect Tournament | Approx Scores | NoMatch

isPerfect :: Matching -> Bool
isPerfect (Perfect _) = True
isPerfect _ = False

{- Tournament ----------------------------------------------------------------}
data Tournament = Tournament { tournamentName :: Name
                             , tournamentURL  :: URL
                             , tournamentType :: TournamentType
                             } deriving (Show, Eq)

instance Ord Tournament where
  compare t1 t2 = compare (tournamentURL t1) (tournamentURL t2)

isPremier :: Tournament -> Bool
isPremier t = tournamentType t == Premier

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
