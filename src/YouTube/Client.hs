{-# LANGUAGE OverloadedStrings #-}

module YouTube.Client
    ( Page(..)
    , PageHandler
    , PageSize
    , Result(..)
    , get
    , post
    , postForm
    , paginate
    , getUserCredentials
    , saveUserCredentials
    ) where

import           Helpers
import           Model                      (URL)
import           Secrets
import           YouTube.Commons

import           Control.Concurrent               (MVar, forkIO, newEmptyMVar, putMVar, takeMVar)
import           Control.Exception                (throwIO)
import           Control.Monad                    (void)
import           Control.Monad.IO.Class           (liftIO)
import           Data.Aeson                       hiding (Result)
import           Data.Aeson.Types                 (typeMismatch)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Char8    as C    (pack, unpack)
import           Data.List                        (unfoldr)
import qualified Data.Text.Lazy           as LT   (pack)
import qualified Data.Traversable         as T    (mapM)
import           Network.HTTP.Simple
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           System.Random                    (randomRIO)
import qualified Web.Scotty               as S    (ActionM, get, param, redirect, scottyApp, html)

{- Types ---------------------------------------------------------------------}
type PageSize = Int
data Page = Page Token PageSize

data Result a = Result Token a

type PageHandler m = Page -> IO (Result m)

{- Functions -----------------------------------------------------------------}
get :: (FromJSON a) => UserCredentials -> URL -> IO a
get = httpRoutine id

httpRoutine :: (FromJSON a) => (Request -> Request) -> UserCredentials -> URL -> IO a
httpRoutine modifier creds url = do
  let request = \c -> (modifier . addCredentials c) <$> parseRequest url >>= httpJSONEither
  getResponseBody <$> (refreshOn401 creds request >>= T.mapM (either throwIO return))

refreshOn401 :: (FromJSON a) => UserCredentials -> (UserCredentials -> IO (Response (Either JSONException a))) -> IO (Response (Either JSONException a))
refreshOn401 NoCredentials action = action NoCredentials -- avoid cycles
refreshOn401 creds action = do
  result <- action creds
  if getResponseStatusCode result == 401
  then oauthRefresh creds >>= saveUserCredentials >>= action
  else return result

addCredentials :: UserCredentials -> Request -> Request
addCredentials (UserCredentials (Token a) _ tt) =
  setRequestIgnoreStatus . addRequestHeader "Authorization" authorization
    where authorization = C.pack $ tt ++ " " ++ a
addCredentials _ = id

post :: (ToJSON b, FromJSON a) => Maybe b -> UserCredentials -> URL -> IO a
post  Nothing    = httpRoutine id
post (Just body) = httpRoutine (setRequestBodyJSON body)

postForm :: FromJSON a => [(ByteString, ByteString)] -> UserCredentials -> URL -> IO a
postForm payload = httpRoutine (setRequestBodyURLEncoded payload)

paginate :: Monoid m => PageHandler m -> PageSize -> IO m
paginate handler count = paginate' handler batches Empty
  where batches = batchBy maxBatchSize count
        maxBatchSize = 50 -- Set by YouTube API

saveUserCredentials :: UserCredentials -> IO UserCredentials
saveUserCredentials c = do
  putStrLn "Saving user credentials"
  let accessToken  = C.pack $ show $ userAccessToken c
  let refreshToken = C.pack $ show $ userRefreshToken c
  void $ storePassword "FTV-CLI YouTube Access Token" accessToken accessTokenAttributes
  void $ storePassword "FTV-CLI YouTube Refresh Token" refreshToken refreshTokenAttributes
  return c

getUserCredentials :: IO UserCredentials
getUserCredentials = do
  currentCreds <- readCurrentUserCredentials
  case currentCreds of
    Just creds -> return creds
    Nothing    -> askUserCredentials >>= saveUserCredentials

{-----------------------------------------------------------------------------}
paginate' :: Monoid m => PageHandler m -> [PageSize] -> Token -> IO m
paginate' _ [] _ = return mempty
paginate' handler (count: counts) token = do
  Result nextToken content <- handler (Page token count)
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

instance (FromJSON a) => FromJSON (Result a) where
  parseJSON value@(Object o) = do
    datum <- parseJSON value
    token <- o .:? "nextPageToken" .!= Empty
    return $ Result token datum
  parseJSON invalid = typeMismatch "Result" invalid

askUserCredentials :: IO UserCredentials
askUserCredentials = do
  port <- randomRIO (32100, 32200)
  let url = oauthAuthorizeUrl port
  putStrLn $ "Your browser should have been open."
  putStrLn $ "If not, please open " ++ url
  openBrowser url
  oauthCredentials port

readCurrentUserCredentials :: IO (Maybe UserCredentials)
readCurrentUserCredentials = do
  accessToken  <- findPassword accessTokenAttributes
  refreshToken <- findPassword [("application", "ftv-cli"), ("type", "refresh_token")]
  return $ UserCredentials <$> (Token . C.unpack <$> accessToken)
                           <*> (Token . C.unpack <$> refreshToken)
                           <*> pure "Bearer"

accessTokenAttributes :: Attributes
accessTokenAttributes = [("application", "ftv-cli"), ("type", "access_token")]

refreshTokenAttributes :: Attributes
refreshTokenAttributes = [("application", "ftv-cli"), ("type", "refresh_token")]

type Port = Int

oauthAuthorizeUrl :: Port -> URL
oauthAuthorizeUrl port = baseURL port ++ "/authorize"

oauthCredentials :: Port -> IO UserCredentials
oauthCredentials port = waitOAuth port >>= requestToken port

oauthRefresh :: UserCredentials -> IO UserCredentials
oauthRefresh (UserCredentials _ (Token refreshToken) _) = do
  putStrLn "Refreshing OAuth token"
  credentials <- getCredentials
  let body = [ ( "client_id"    , C.pack $ clientId credentials     )
             , ( "client_secret", C.pack $ clientSecret credentials )
             , ( "refresh_token", C.pack $ refreshToken             )
             , ( "grant_type"   , "refresh_token"                   )
             ]
  creds <- postForm body NoCredentials "POST https://accounts.google.com/o/oauth2/token"
  return $ creds { userRefreshToken = Token refreshToken }
oauthRefresh _ = return NoCredentials

application :: URL -> MVar String -> IO Wai.Application
application authUrl returnValue = S.scottyApp $ do
  S.get "/authorize" (shortenerHandler authUrl)
  S.get "/"          (callbackHandler returnValue)

shortenerHandler :: String -> S.ActionM ()
shortenerHandler url = S.redirect (LT.pack url)

callbackHandler :: MVar String -> S.ActionM ()
callbackHandler returnValue = do
  token <- S.param "code"
  liftIO (putMVar returnValue token)
  S.html "<html><body><strong>Thank you!</strong><br/><br/>Please get back to your terminal to continue.</body></html>"

awaitCallback :: Int -> URL -> IO String
awaitCallback port authUrl = do
  returnValue <- newEmptyMVar
  app <- application authUrl returnValue
  _ <- forkIO $ Warp.run port app
  takeMVar returnValue

getOAuthUrl :: Int -> IO String
getOAuthUrl port = do
  credentials <- getCredentials
  return $ mkUrl "https://accounts.google.com/o/oauth2/v2/auth"
    [ ( "response_type" , "code"                                    )
    , ( "client_id"     , clientId credentials                      )
    , ( "redirect_uri"  , baseURL port                              )
    , ( "scope"         , "https://www.googleapis.com/auth/youtube" )
    ]

waitOAuth :: Port -> IO String
waitOAuth port = getOAuthUrl port >>= awaitCallback port

requestToken :: Int -> String -> IO UserCredentials
requestToken port personalToken = do
  credentials <- getCredentials
  post noBody NoCredentials $ mkUrl "POST https://www.googleapis.com/oauth2/v4/token"
    [ ( "code"          , personalToken            )
    , ( "client_id"     , clientId credentials     )
    , ( "client_secret" , clientSecret credentials )
    , ( "redirect_uri"  , baseURL port             )
    , ( "grant_type"    , "authorization_code"     )
    ]

baseURL :: Port -> URL
baseURL port = "http://localhost:" ++ show port
