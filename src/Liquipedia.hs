module Liquipedia
    ( listTournaments, lastTournaments
    ) where

import Model
import Data.RDF
import qualified Data.Text as T
import Data.List (deleteFirstsBy)

fullURL = (++) "http://wiki.teamliquid.net"

listTournaments :: IO (Maybe [Tournament])
listTournaments = merge <$> (getTournaments Premier) <*> (getTournaments Standard)

merge :: Maybe [Tournament] -> Maybe [Tournament] -> Maybe [Tournament]
merge premierTournaments Nothing = premierTournaments
merge Nothing _ = Nothing
merge premierTournaments allTournaments = fuse <$> premierTournaments <*> allTournaments

fuse :: [Tournament] -> [Tournament] -> [Tournament]
fuse premiers alls = premiers ++ deleteFirstsBy (\t1 t2 -> tournamentURL t1 == tournamentURL t2) alls premiers

getTournaments :: TournamentType -> IO (Maybe [Tournament])
getTournaments tournamentType = do
  graph <- getRDF tournamentType
  let nodes = map subjectOf $ query graph Nothing (someUrl "rdf:type") (someUrl $ category tournamentType)
  return $ Just $ map (searchTournament graph) nodes
  where someUrl = Just . unode . T.pack
        category Premier = "&wiki;Category-3APremier_Tournaments"
        category Standard = "&wiki;Category-3ATournaments"
        searchTournament g n = Tournament (searchText g n) (searchURL g n) tournamentType
        searchText g n = map unslash $ readText $ searchProperty "rdfs:label" g n
        searchURL g n = readURL $ searchProperty "swivt:page" g n
        searchProperty url g n = head $ map objectOf $ query g (Just n) (someUrl url) Nothing

unslash :: Char -> Char
unslash '/' = ' '
unslash  c  =  c

readURL :: Node -> Model.URL
readURL (UNode t) = T.unpack t

readText :: Node -> String
readText (LNode (PlainL t)) = T.unpack t

getRDF :: TournamentType -> IO TriplesList
getRDF tournamentType = fromEither <$> parseURL parser (url tournamentType)
  where url Premier = fullURL "/dota2/index.php?title=Special:ExportRDF/Category:Premier_Tournaments&xmlmime=rdf"
        url Standard = fullURL "/dota2/index.php?title=Special:ExportRDF/Category:Tournaments&xmlmime=rdf"
        parser = XmlParser Nothing Nothing

-- For testing purposes
lastTournaments :: Int -> IO ()
lastTournaments n | n <= 0    = putStrLn "No tournament"
lastTournaments n | otherwise = do
  say n
  tournaments <- listTournaments
  prettyPrintTournaments $ take n <$> tournaments

say :: Int -> IO ()
say 1 = putStrLn "Last tournament"
say n = putStrLn $ "Last " ++ (show n) ++ " tournaments"

prettyPrintTournaments :: Maybe [Tournament] -> IO ()
prettyPrintTournaments Nothing   = putStrLn "none"
prettyPrintTournaments (Just ts) = putStr . unlines . map showName $ ts
  where showName = tournamentName
