module Main where

import Lib
import Model
import Liquipedia
import Matcher

import Data.Maybe (fromJust)
import Data.List (find)
import Control.Monad (forM_)

import System.Environment (getEnv)

main :: IO ()
main = do
  apiKey <- getEnv "API_KEY"
  listPlaylistItems apiKey uploadsPlaylistId >>= prettyPrintPlaylistContent

uploadsPlaylistId = "UUHmNTOzvZhZwaRJoioK0Mqw"

attemptMatching :: YoutubeId -> IO ()
attemptMatching id =
  do
    apiKey <- getEnv "API_KEY"
    tournaments <- fromJust <$> listTournaments
    videoDetails <- videoDetails <$> listPlaylistItems apiKey uploadsPlaylistId
    let video@(VideoDetails title _) = fromJust $ find ((== id) . videoId) videoDetails
    putStrLn title
    prettyPrint $ matchTournaments video tournaments

matchings :: IO ()
matchings = forM_ ids attemptMatching
  where ids = [ "XiLdRr23FFw"
              , "HNc61ZU3WT4"
              , "Th9BihtgGzc"
              , "X2E9VYP1Hwg"]

prettyPrint :: [(Score, Tournament)] -> IO ()
prettyPrint = putStr . unlines . map prettyPrintScore

prettyPrintScore :: (Score, Tournament) -> String
prettyPrintScore (s, (Tournament t _)) = " " ++ scoreAsPercenage s ++ " " ++ t
  where scoreAsPercenage s = (show $ truncate $ (fromRational s * 100 :: Float)) ++ "%"
