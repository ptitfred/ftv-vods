module Liquipedia
    ( Tournament(..)
    , listTournaments, lastTournaments
    ) where

import Model
import Text.HTML.Scalpel

fullURL = (++) "http://wiki.teamliquid.net"

listTournaments :: IO (Maybe [Tournament])
listTournaments = scrapeURL url tournaments
  where url = fullURL "/dota2/Portal:Tournaments"

tournaments :: Scraper String [Tournament]
tournaments = chroots selector tournament
  where selector = "div" @: ["id" @= "mw-content-text"] // "table" // "ul" // "li"

tournament :: Scraper String Tournament
tournament = Tournament <$> text "a" <*> link

link :: Scraper String Model.URL
link = fullURL <$> attr "href" "a"

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
  where showName (Tournament name _) = name
