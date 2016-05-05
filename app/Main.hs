module Main where

import Lib
import Model
import Liquipedia
import Matcher

import Data.Maybe (fromJust)
import Data.List (find)
import Control.Monad (forM_)

main :: IO ()
main = listPlaylistItems apiKey uploadsPlaylistId >>= prettyPrintPlaylistContent

-- Configuration
-- Should be read from environment variables
apiKey = "AIzaSyCec4oVXaalTcu5qC7JrAPAGzyaHtwRojU"
uploadsPlaylistId = "UUHmNTOzvZhZwaRJoioK0Mqw"
-- ftvChannelId = "UCHmNTOzvZhZwaRJoioK0Mqw"
--

attemptMatching :: YoutubeId -> IO ()
attemptMatching id =
  do
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
