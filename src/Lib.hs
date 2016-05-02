module Lib
    ( listPlaylistItems, prettyPrintPlaylistContent
    ) where

import Model
import Data.List (intercalate)
import Network.HTTP.Simple
import Data.Aeson (FromJSON)

getJSON :: (FromJSON a) => String -> IO a
getJSON url = parseRequest url >>= fmap getResponseBody . httpJSON

listPlaylistItems :: ApiKey -> YoutubeId -> IO PlaylistContent
listPlaylistItems apiKey playlistId = getJSON (buildUrl url parameters)
  where url = "https://www.googleapis.com/youtube/v3/playlistItems"
        parameters = [ ("part"      , "contentDetails,snippet")
                     , ("maxResults", "50"                    )
                     , ("playlistId", playlistId              )
                     , ("key"       , apiKey                  )]

buildUrl :: String -> [(String, String)] -> String
buildUrl url []         = url
buildUrl url parameters = url ++ "?" ++ toParameters parameters
  where toParameters = intercalate "&" . map toParameter
        toParameter (a, b) = a ++ "=" ++ b

prettyPrintPlaylistContent :: PlaylistContent -> IO ()
prettyPrintPlaylistContent (PlaylistContent videoDetails) =
  putStr . unlines . map showTitle $ videoDetails
    where showTitle (VideoDetails title _) = title
