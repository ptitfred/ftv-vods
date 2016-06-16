module Liquipedia
    ( listTournaments
    ) where

import Model

import Data.List (deleteFirstsBy)
import Data.RDF
import qualified Data.Text as T

fullURL :: String -> String
fullURL = (++) "http://wiki.teamliquid.net"

listTournaments :: IO (Maybe [Tournament])
listTournaments = merge <$> getTournaments Premier <*> getTournaments Standard

merge :: Maybe [Tournament] -> Maybe [Tournament] -> Maybe [Tournament]
merge premierTournaments Nothing = premierTournaments
merge Nothing _ = Nothing
merge premierTournaments allTournaments = fuse <$> premierTournaments <*> allTournaments

fuse :: [Tournament] -> [Tournament] -> [Tournament]
fuse premiers alls = premiers ++ nonPremiers
  where nonPremiers = deleteFirstsBy (\t1 t2 -> tournamentURL t1 == tournamentURL t2) alls premiers

getTournaments :: TournamentType -> IO (Maybe [Tournament])
getTournaments t = readTournaments t <$> getRDF t

readTournaments :: RDF rdf => TournamentType -> Maybe rdf -> Maybe [Tournament]
readTournaments _ Nothing = Nothing
readTournaments t (Just graph) =
  return $ map (readTournament t graph) nodes
    where nodes = map subjectOf $ query graph Nothing withType (category t)

readTournament :: RDF rdf => TournamentType -> rdf -> Node -> Tournament
readTournament t g n = Tournament queryLabel queryURL t
  where queryLabel = map unslash $ readText $ searchProperty "rdfs:label"
        queryURL = readURL $ searchProperty "swivt:page"
        searchProperty url = head $ map objectOf $ query g (Just n) (someUrl url) Nothing

withType :: Maybe Node
withType = someUrl "rdf:type"

someUrl :: String -> Maybe Node
someUrl = Just . unode . T.pack

category :: TournamentType -> Maybe Node
category Premier  = someUrl "&wiki;Category-3APremier_Tournaments"
category Standard = someUrl "&wiki;Category-3ATournaments"

unslash :: Char -> Char
unslash '/' = ' '
unslash  c  =  c

readURL :: Node -> Model.URL
readURL (UNode t) = T.unpack t
readURL _ = ""

readText :: Node -> String
readText (LNode (PlainL t)) = T.unpack t
readText _ = ""

getRDF :: TournamentType -> IO (Maybe TriplesList)
getRDF t = (Just . fromEither) <$> parseURL parser (url t)
  where url Premier  = fullURL "/dota2/Special:ExportRDF/Category:Premier_Tournaments"
        url Standard = fullURL "/dota2/Special:ExportRDF/Category:Tournaments"
        parser = XmlParser Nothing Nothing
