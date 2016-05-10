{-# LANGUAGE OverloadedStrings #-}

module YouTube
    ( listPlaylistItems
    ) where

import Model

import Data.Aeson
import Data.Char (isAlphaNum)
import Data.List (find, intercalate)
import Data.List.Split (wordsBy)
import Data.Maybe (catMaybes)
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

instance FromJSON VideoDetails where
  parseJSON (Object object) = do
    snippet        <- object .: "snippet"
    contentDetails <- object .: "contentDetails"
    title          <- snippet .: "title"
    videoId        <- contentDetails .: "videoId"
    description    <- snippet .: "description"
    let casters = extractCasters description
    return $ VideoDetails title videoId description casters

extractCasters :: String -> [Caster]
extractCasters description = catMaybes $ map nameToCaster $ filter (`elem` (concatMap casterPseudos casters)) $ tokenize description

tokenize :: String -> [String]
tokenize = wordsBy sep
  where sep = not.isAlphaNum

nameToCaster :: Name -> Maybe Caster
nameToCaster name = find (isCaster name) casters

instance FromJSON PlaylistContent where
  parseJSON (Object object) = PlaylistContent <$> object .: "items"
