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

listPlaylistItems :: ApiKey -> YoutubeId -> Int -> IO PlaylistContent
listPlaylistItems apiKey playlistId count = paginate handler count
  where handler  = listPlaylistItemsAction apiKey playlistId

listPlaylistItemsAction :: ApiKey -> YoutubeId -> Int -> Maybe PageToken -> IO (YoutubeResponse PlaylistContent)
listPlaylistItemsAction apiKey playlistId count pageToken = getJSON (buildUrl url parameters)
  where url = "https://www.googleapis.com/youtube/v3/playlistItems"
        parameters = [ ("part"      , "contentDetails,snippet")
                     , ("maxResults", show count              )
                     , ("playlistId", playlistId              )
                     , ("key"       , apiKey                  )
                     , ("pageToken" , token pageToken         )
                     ]
        token Nothing = ""
        token (Just (PageToken t)) = t

instance FromJSON VideoDetails where
  parseJSON (Object object) = do
    snippet        <- object .: "snippet"
    contentDetails <- object .: "contentDetails"
    title          <- snippet .: "title"
    videoId        <- contentDetails .: "videoId"
    description    <- snippet .: "description"
    publishDate    <- snippet .: "publishedAt"
    let casters = extractCasters description
    let url = "https://www.youtube.com/watch?v=" ++ videoId
    return $ VideoDetails title videoId description casters url publishDate

extractCasters :: String -> [Caster]
extractCasters description = catMaybes $ map nameToCaster $ filter (`elem` (concatMap casterPseudos casters)) $ tokenize description

tokenize :: String -> [String]
tokenize = wordsBy sep
  where sep = not.isAlphaNum

nameToCaster :: Name -> Maybe Caster
nameToCaster name = find (isCaster name) casters

instance FromJSON PlaylistContent where
  parseJSON (Object object) = PlaylistContent <$> object .: "items"
  parseJSON _               = return mempty

{- Youtube API helpers -------------------------------------------------------}

getJSON :: (FromJSON a) => String -> IO a
getJSON url = getResponseBody <$> (parseRequest url >>= httpJSON)

type PageSize = Int

type PageHandler m = (PageSize -> Maybe PageToken -> IO (YoutubeResponse m))

paginate :: Monoid m => PageHandler m -> PageSize -> IO m
paginate handler count = paginate' handler batches Nothing
  where batches = batchBy maxBatchSize count
        maxBatchSize = 50 -- Set by YouTube API

paginate' :: Monoid m => PageHandler m -> [PageSize] -> Maybe PageToken -> IO m
paginate' handler [] _ = return mempty
paginate' handler (count: counts) token = do
  YoutubeResponse nextToken content <- handler count token
  (content <>) <$> paginate' handler counts nextToken

batchBy :: PageSize -> PageSize -> [PageSize]
batchBy batchSize = unfoldr (cutBatch batchSize)

cutBatch :: PageSize -> PageSize -> Maybe (PageSize, PageSize)
cutBatch batchSize count | count <= 0 = Nothing
                         | otherwise  = Just (nextBatch, remaining)
  where nextBatch = batchSize `min` count
        remaining = count - nextBatch

newtype PageToken = PageToken String deriving (Show)

buildUrl :: String -> [(String, String)] -> String
buildUrl url []         = url
buildUrl url parameters = url ++ "?" ++ toParameters parameters
  where toParameters :: [(String, String)] -> String
        toParameters = intercalate "&" . map toParameter
        toParameter :: (String, String) -> String
        toParameter (a, b) = a ++ "=" ++ b

data YoutubeResponse a = YoutubeResponse (Maybe PageToken) a

instance FromJSON a => FromJSON (YoutubeResponse a) where
  parseJSON value@(Object object) = do
    datum <- parseJSON value
    token <- avoidEmptyToken <$> object .:? "nextPageToken"
    return $ YoutubeResponse token datum

avoidEmptyToken :: Maybe PageToken -> Maybe PageToken
avoidEmptyToken (Just (PageToken "")) = Nothing
avoidEmptyToken value = value

instance FromJSON PageToken where
  parseJSON (String s) = return $ PageToken $ T.unpack s
