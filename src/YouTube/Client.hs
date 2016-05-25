{-# LANGUAGE OverloadedStrings #-}

module YouTube.Client
    ( Page(..)
    , PageHandler
    , PageSize
    , Response(..)
    , get
    , post
    , paginate
    ) where

import           Model (URL)
import           YouTube.Commons

import           Data.Aeson hiding (object)
import           Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Char8 as B
import qualified Data.CaseInsensitive as CI
import           Data.List (unfoldr)
import           Network.HTTP.Simple hiding (Response)

{- Types ---------------------------------------------------------------------}
type PageSize = Int
data Page = Page Token PageSize

data Response a = Response Token a

type PageHandler m = Page -> IO (Response m)

{- Functions -----------------------------------------------------------------}
get :: (FromJSON a) => UserCredentials -> URL -> IO a
get = httpRoutine id

httpRoutine :: (FromJSON a) => (Request -> Request) -> UserCredentials -> URL -> IO a
httpRoutine modifier creds url = do
  request <- (modifier . addCredentials creds) <$> parseRequest url
  getResponseBody <$> httpJSON request

addCredentials :: UserCredentials -> Request -> Request
addCredentials (UserCredentials (Token a) _ tt) = addRequestHeader (CI.mk "Authorization") authorization
  where authorization = B.pack $ tt ++ " " ++ a
addCredentials _ = id

post :: (ToJSON b, FromJSON a) => Maybe b -> UserCredentials -> URL ->  IO a
post Nothing = httpRoutine id
post (Just body) = httpRoutine (setRequestBodyJSON body)

paginate :: Monoid m => PageHandler m -> PageSize -> IO m
paginate handler count = paginate' handler batches Empty
  where batches = batchBy maxBatchSize count
        maxBatchSize = 50 -- Set by YouTube API

{-----------------------------------------------------------------------------}
paginate' :: Monoid m => PageHandler m -> [PageSize] -> Token -> IO m
paginate' _ [] _ = return mempty
paginate' handler (count: counts) token = do
  Response nextToken content <- handler (Page token count)
  case nextToken of
    Empty -> return content
    _     -> mappend content <$> paginate' handler counts nextToken

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
    token <- object .:? "nextPageToken" .!= Empty
    return $ Response token datum
  parseJSON invalid = typeMismatch "Response" invalid
