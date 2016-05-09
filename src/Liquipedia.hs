module Liquipedia
    ( listTournaments
    ) where

import Model

import Data.List (deleteFirstsBy)
import Data.RDF
import qualified Data.Text as T

fullURL = (++) "http://wiki.teamliquid.net"

listTournaments :: IO (Maybe [Tournament])
listTournaments = merge <$> (getTournaments Premier) <*> (getTournaments Standard)

merge :: Maybe [Tournament] -> Maybe [Tournament] -> Maybe [Tournament]
merge premierTournaments Nothing = premierTournaments
merge Nothing _ = Nothing
merge premierTournaments allTournaments = fuse <$> premierTournaments <*> allTournaments

fuse :: [Tournament] -> [Tournament] -> [Tournament]
fuse premiers alls = premiers ++ nonPremiers
  where nonPremiers = deleteFirstsBy (\t1 t2 -> tournamentURL t1 == tournamentURL t2) alls premiers

getTournaments :: TournamentType -> IO (Maybe [Tournament])
getTournaments tournamentType =
  readTournaments tournamentType <$> getRDF tournamentType

readTournaments :: RDF rdf => TournamentType -> Maybe rdf -> Maybe [Tournament]
readTournaments _ Nothing = Nothing
readTournaments tournamentType (Just graph) =
  return $ map (readTournament tournamentType graph) nodes
    where nodes = map subjectOf $ query graph Nothing withType (category tournamentType)

readTournament :: RDF rdf => TournamentType -> rdf -> Node -> Tournament
readTournament tournamentType g n = Tournament (queryLabel g n) (queryURL g n) tournamentType
  where queryLabel g n = map unslash $ readText $ searchProperty "rdfs:label" g n
        queryURL g n = readURL $ searchProperty "swivt:page" g n
        searchProperty url g n = head $ map objectOf $ query g (Just n) (someUrl url) Nothing

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

readText :: Node -> String
readText (LNode (PlainL t)) = T.unpack t

getRDF :: TournamentType -> IO (Maybe TriplesList)
getRDF tournamentType = (Just . fromEither) <$> parseURL parser (url tournamentType)
  where url Premier  = fullURL "/dota2/index.php?title=Special:ExportRDF/Category:Premier_Tournaments&xmlmime=rdf"
        url Standard = fullURL "/dota2/index.php?title=Special:ExportRDF/Category:Tournaments&xmlmime=rdf"
        parser = XmlParser Nothing Nothing
