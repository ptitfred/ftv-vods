{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( listPlaylistItems, prettyPrintPlaylistContent
    ) where

import Model

import Data.Aeson
import Data.List (intercalate)
import Network.HTTP.Simple

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
  where toParameters :: [(String, String)] -> String
        toParameters = intercalate "&" . map toParameter
        toParameter :: (String, String) -> String
        toParameter (a, b) = a ++ "=" ++ b

prettyPrintPlaylistContent :: PlaylistContent -> IO ()
prettyPrintPlaylistContent (PlaylistContent videoDetails) =
  putStr . unlines . map showTitle $ videoDetails
    where showTitle (VideoDetails title _ _) = title

instance FromJSON VideoDetails where
  parseJSON (Object object) = do
    snippet        <- object .: "snippet"
    contentDetails <- object .: "contentDetails"
    VideoDetails <$> snippet .: "title" <*> contentDetails .: "videoId" <*> snippet .: "description"

instance FromJSON PlaylistContent where
  parseJSON (Object object) = PlaylistContent <$> object .: "items"
