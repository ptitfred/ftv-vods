{-# LANGUAGE OverloadedStrings #-}

module YouTube.Client
    ( Credentials
    , Page(..)
    , PageHandler
    , PageSize
    , Parameter
    , Parameters
    , Token
    , Response(..)
    , apiKey
    , getCredentials
    , mkUrl
    , get
    , paginate
    ) where

import Model (URL)

import Data.Aeson hiding (object)
import Data.Aeson.Types (typeMismatch)
import Data.List (intercalate, unfoldr)
import qualified Data.Text as T (unpack)
import Network.HTTP.Simple hiding (Response)
import System.Environment (getEnv)

{- Types ---------------------------------------------------------------------}
data Credentials = Credentials ApiKey OAuthClientId OAuthSecret
type PageSize = Int
data Token = Empty | Token String
data Page = Page Token PageSize

data Response a = Response Token a

type PageHandler m = Page -> IO (Response m)

type Parameters = [Parameter]
type Parameter = (String, String)

{- Functions -----------------------------------------------------------------}
getCredentials :: IO Credentials
getCredentials = Credentials <$> getEnv "API_KEY"
                             <*> getEnv "OAUTH_CLIENT_ID"
                             <*> getEnv "OAUTH_CLIENT_SECRET"

apiKey :: Credentials -> String
apiKey (Credentials key _ _) = key

get :: (FromJSON a) => URL -> IO a
get url = getResponseBody <$> (parseRequest url >>= httpJSON)

instance Show Token where
  show  Empty    = ""
  show (Token t) = t

mkUrl :: URL -> Parameters -> URL
mkUrl url []         = url
mkUrl url parameters = url ++ "?" ++ toParameters parameters

paginate :: Monoid m => PageHandler m -> PageSize -> IO m
paginate handler count = paginate' handler batches Empty
  where batches = batchBy maxBatchSize count
        maxBatchSize = 50 -- Set by YouTube API

{-----------------------------------------------------------------------------}
type ApiKey = String
type OAuthClientId = String
type OAuthSecret = String

mkToken :: String -> Token
mkToken "" = Empty
mkToken s  = Token s

toParameters :: Parameters -> String
toParameters = intercalate "&" . map toParameter

toParameter :: Parameter -> String
toParameter (a, b) = a ++ "=" ++ b

paginate' :: Monoid m => PageHandler m -> [PageSize] -> Token -> IO m
paginate' _ [] _ = return mempty
paginate' handler (count: counts) token = do
  Response nextToken content <- handler (Page token count)
  mappend content <$> paginate' handler counts nextToken

batchBy :: PageSize -> PageSize -> [PageSize]
batchBy batchSize = unfoldr (cutBatch batchSize)

cutBatch :: PageSize -> PageSize -> Maybe (PageSize, PageSize)
cutBatch batchSize count | count <= 0 = Nothing
                         | otherwise  = Just (nextBatch, remaining)
  where nextBatch = batchSize `min` count
        remaining = count - nextBatch

instance (FromJSON a) => FromJSON (Response a) where
  parseJSON value@(Object object) = do
    datum <- parseJSON value
    token <- object .: "nextPageToken"
    return $ Response token datum
  parseJSON invalid = typeMismatch "Response" invalid

instance FromJSON Token where
  parseJSON (String s) = return $ mkToken $ T.unpack s
  parseJSON invalid = typeMismatch "Token" invalid
