{-# LANGUAGE OverloadedStrings #-}

module YouTube
    ( listPlaylistItems
    ) where

import Model

import Data.Aeson
import Data.Char (isAlphaNum)
import Data.List (find, intercalate, unfoldr)
import Data.List.Split (wordsBy)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import qualified Data.Text as T (unpack)
import Network.HTTP.Simple

getJSON :: (FromJSON a) => String -> IO a
getJSON url = getResponseBody <$> (parseRequest url >>= httpJSON)

listPlaylistItems :: ApiKey -> YoutubeId -> Int -> IO PlaylistContent
listPlaylistItems apiKey playlistId count = listPlaylistItems' Nothing apiKey playlistId batches
  where batches = batchBy maxBatchSize count

listPlaylistItems' :: Maybe PageToken -> ApiKey -> YoutubeId -> [Int] -> IO PlaylistContent
listPlaylistItems' token apiKey playlistId [] = return mempty
listPlaylistItems' token apiKey playlistId (count: counts) = do
  PlaylistResponse (content, nextToken) <- listPlaylistItemsAction token apiKey playlistId count
  (content <>) <$> listPlaylistItems' nextToken apiKey playlistId counts

-- Set by YouTube API
maxBatchSize :: Int
maxBatchSize = 50

batchBy :: Int -> Int -> [Int]
batchBy batchSize = unfoldr (cutBatch batchSize)

cutBatch :: Int -> Int -> Maybe (Int, Int)
cutBatch batchSize count | count <= 0 = Nothing
                         | otherwise  = Just (nextBatch, remaining)
  where nextBatch = batchSize `min` count
        remaining = count - nextBatch

listPlaylistItemsAction :: Maybe PageToken -> ApiKey -> YoutubeId -> Int -> IO PlaylistResponse
listPlaylistItemsAction pageToken apiKey playlistId count = getJSON (buildUrl url parameters)
  where url = "https://www.googleapis.com/youtube/v3/playlistItems"
        parameters = [ ("part"      , "contentDetails,snippet")
                     , ("maxResults", show count              )
                     , ("playlistId", playlistId              )
                     , ("key"       , apiKey                  )
                     , ("pageToken" , token pageToken         )
                     ]
        token Nothing = ""
        token (Just (PageToken t)) = t

buildUrl :: String -> [(String, String)] -> String
buildUrl url []         = url
buildUrl url parameters = url ++ "?" ++ toParameters parameters
  where toParameters :: [(String, String)] -> String
        toParameters = intercalate "&" . map toParameter
        toParameter :: (String, String) -> String
        toParameter (a, b) = a ++ "=" ++ b

newtype PageToken = PageToken String deriving (Show)

newtype PlaylistResponse = PlaylistResponse (PlaylistContent, Maybe PageToken)

instance FromJSON PlaylistResponse where
  parseJSON (Object object) = do
    playlistContent <- parseJSON (Object object)
    pageToken       <- object .:? "nextPageToken"
    return $ PlaylistResponse (playlistContent, avoidEmptyToken pageToken)

avoidEmptyToken :: Maybe PageToken -> Maybe PageToken
avoidEmptyToken (Just (PageToken "")) = Nothing
avoidEmptyToken value = value

instance FromJSON PageToken where
  parseJSON (String s) = return $ PageToken $ T.unpack s

instance FromJSON VideoDetails where
  parseJSON (Object object) = do
    snippet        <- object .: "snippet"
    contentDetails <- object .: "contentDetails"
    title          <- snippet .: "title"
    videoId        <- contentDetails .: "videoId"
    description    <- snippet .: "description"
    let casters = extractCasters description
    let url = "https://www.youtube.com/watch?v=" ++ videoId
    return $ VideoDetails title videoId description casters url

extractCasters :: String -> [Caster]
extractCasters description = catMaybes $ map nameToCaster $ filter (`elem` (concatMap casterPseudos casters)) $ tokenize description

tokenize :: String -> [String]
tokenize = wordsBy sep
  where sep = not.isAlphaNum

nameToCaster :: Name -> Maybe Caster
nameToCaster name = find (isCaster name) casters

instance FromJSON PlaylistContent where
  parseJSON (Object object) = PlaylistContent <$> object .: "items"
