{-# LANGUAGE OverloadedStrings #-}

module GoogleAPIsClient.Client
    ( Client
    , Endpoint(..)
    , Page(..)
    , PageHandler
    , PageSize
    , Result(..)
    , runClient
    , liftIO
    , needsUserCredentials
    , get
    , post
    , postForm
    , delete
    , paginate
    , withPage
    ) where

import           GoogleAPIsClient.Commons
import           GoogleAPIsClient.Secrets

import           Control.Concurrent               (MVar, forkIO, newEmptyMVar, putMVar, takeMVar)
import           Control.Exception                (throwIO)
import           Control.Monad                    (void)
import qualified Control.Monad.Reader     as RM   (ReaderT, ask, asks, runReaderT)
import qualified Control.Monad.State      as STM  (StateT, get, put, runStateT)
import           Control.Monad.IO.Class           (liftIO)
import           Data.Aeson                       hiding (Result)
import           Data.Aeson.Types                 (typeMismatch)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Char8    as C    (pack, unpack)
import           Data.List                        (unfoldr, isPrefixOf)
import qualified Data.Text.Lazy           as LT   (pack)
import qualified Data.Traversable         as T    (mapM)
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status        (statusIsSuccessful)
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           System.Environment               (getEnv)
import           System.Random                    (randomRIO)
import qualified Web.Scotty               as S    (ActionM, get, param, redirect, scottyApp, html)

{- Types ---------------------------------------------------------------------}
type PageSize = Int
data Page = Page Token PageSize

data Result a = Result Token a

data Endpoint = Endpoint { baseUrl :: URL
                         , scope   :: URL
                         }

type PageHandler m = Page -> Client (Result m)
type Client = RM.ReaderT (Endpoint, Credentials) (STM.StateT UserCredentials IO)

{- Functions -----------------------------------------------------------------}

runClient :: Endpoint -> Client a -> IO a
runClient endpoint client = do
  credentials <- getCredentials
  fst <$> STM.runStateT (RM.runReaderT client (endpoint, credentials)) NoCredentials

needsUserCredentials :: Client ()
needsUserCredentials = do
  credentials <- STM.get
  case credentials of
    NoCredentials -> getUserCredentials
    _ -> return ()

get :: (FromJSON a) => URL -> Parameters -> Client a
get u ps = mkURL "GET" u ps >>= httpRoutine id

post :: (ToJSON b, FromJSON a) => URL -> Parameters -> Maybe b -> Client a
post u ps  Nothing    = mkURL "POST" u ps >>= httpRoutine id
post u ps (Just body) = mkURL "POST" u ps >>= httpRoutine (setRequestBodyJSON body)

postForm :: FromJSON a => URL -> Parameters -> [(ByteString, ByteString)] -> Client a
postForm u ps payload = mkURL "POST" u ps >>= httpRoutine (setRequestBodyURLEncoded payload)

mkURL :: String -> URL -> Parameters -> Client URL
mkURL verb u ps = do
  Endpoint base _ <- RM.asks fst
  key <- apiKey <$> RM.asks snd
  return $ mkUrl (verb ++ " " ++ fullURL base u) (("key", key) : ps)

fullURL :: URL -> URL -> URL
fullURL _ url  | "http" `isPrefixOf` url = url
fullURL base path = base ++ path

delete :: URL -> Parameters -> Client Bool
delete url parameters = do
  creds <- STM.get
  u <- mkURL "DELETE" url parameters
  prepare creds u >>= handle
  where prepare c u = addCredentials c <$> parseRequest u
        handle r = isSuccess <$> httpLBS r
        isSuccess = statusIsSuccessful . getResponseStatus

paginate :: Monoid m => PageHandler m -> PageSize -> Client m
paginate handler count = paginate' handler batches Empty
  where batches = batchBy maxBatchSize count
        maxBatchSize = 50 -- Set by YouTube API

withPage :: Page -> Parameters -> Parameters
withPage (Page token count) parameters =
  ("pageToken" , show token) : ("maxResults", show count) : parameters

getUserCredentials :: Client ()
getUserCredentials = do
  currentCreds <- liftIO readCurrentUserCredentials
  case currentCreds of
    Nothing    -> askUserCredentials >> saveUserCredentials
    Just creds -> STM.put creds

saveUserCredentials :: Client ()
saveUserCredentials = do
  creds <- STM.get
  let accessToken  = C.pack $ show $ userAccessToken creds
  let refreshToken = C.pack $ show $ userRefreshToken creds
  liftIO $ do
    putStrLn "Saving user credentials"
    appCode <- getEnv "APPLICATION_CODE"
    appName <- getEnv "APPLICATION_NAME"
    void $ storePassword (appName ++ " Access Token") accessToken (accessTokenAttributes appCode)
    void $ storePassword (appName ++ " Refresh Token") refreshToken (refreshTokenAttributes appCode)
  return ()

{- Implementation ------------------------------------------------------------}
httpRoutine :: (FromJSON a) => (Request -> Request) -> URL -> Client a
httpRoutine modifier url = do
  let request = prepareRequest modifier url
  let action = refreshOn401 request >>= T.mapM (either (liftIO.throwIO) return)
  getResponseBody <$> action

prepareRequest :: (FromJSON a) => (Request -> Request) -> URL -> Client (Response (Either JSONException a))
prepareRequest modifier url = do
  c <- STM.get
  some <- (modifier . addCredentials c) <$> parseRequest url
  liftIO $ httpJSONEither some

refreshOn401 :: Client (Response a) -> Client (Response a)
refreshOn401 action = do
  creds <- STM.get
  case creds of
    NoCredentials -> action
    _ -> do
      result <- action
      if getResponseStatusCode result == 401
      then oauthRefresh >> saveUserCredentials >> action
      else return result

addCredentials :: UserCredentials -> Request -> Request
addCredentials (UserCredentials (Token a) _ tt) =
  setRequestIgnoreStatus . addRequestHeader "Authorization" authorization
    where authorization = C.pack $ tt ++ " " ++ a
addCredentials _ = id

{-----------------------------------------------------------------------------}
paginate' :: Monoid m => PageHandler m -> [PageSize] -> Token -> Client m
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

askUserCredentials :: Client ()
askUserCredentials = do
  port <- liftIO $ randomRIO (32100, 32200)
  let url = oauthAuthorizeUrl port
  liftIO $ do
    putStrLn "Your browser should have been open."
    putStrLn $ "If not, please open " ++ url
    openBrowser url
  oauthCredentials port

readCurrentUserCredentials :: IO (Maybe UserCredentials)
readCurrentUserCredentials = do
  appCode      <- getEnv "APPLICATION_CODE"
  accessToken  <- findPassword (accessTokenAttributes appCode)
  refreshToken <- findPassword [("application", appCode), ("type", "refresh_token")]
  return $ UserCredentials <$> (Token . C.unpack <$> accessToken)
                           <*> (Token . C.unpack <$> refreshToken)
                           <*> pure "Bearer"

accessTokenAttributes :: String -> Attributes
accessTokenAttributes appCode = [("application", appCode), ("type", "access_token")]

refreshTokenAttributes :: String -> Attributes
refreshTokenAttributes appCode = [("application", appCode), ("type", "refresh_token")]

type Port = Int

oauthAuthorizeUrl :: Port -> URL
oauthAuthorizeUrl port = baseURL port ++ "/authorize"

oauthCredentials :: Port -> Client ()
oauthCredentials port = waitOAuth port >>= requestToken port

oauthRefresh :: Client ()
oauthRefresh  = do
  creds <- STM.get
  case creds of
    UserCredentials _ (Token refreshToken) _ -> do
      liftIO $ putStrLn "Refreshing OAuth token"
      credentials <- RM.asks snd
      let body = [ ( "client_id"    , C.pack $ clientId credentials     )
                 , ( "client_secret", C.pack $ clientSecret credentials )
                 , ( "refresh_token", C.pack refreshToken               )
                 , ( "grant_type"   , "refresh_token"                   )
                 ]
      STM.put NoCredentials
      creds' <- postForm "https://accounts.google.com/o/oauth2/token" [] body
      STM.put creds' { userRefreshToken = Token refreshToken }
      return ()
    _ -> return ()

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

awaitCallback :: Int -> URL -> Client String
awaitCallback port authUrl =
  liftIO $ do
    returnValue <- newEmptyMVar
    app <- application authUrl returnValue
    _ <- forkIO $ Warp.run port app
    takeMVar returnValue

getOAuthUrl :: Int -> Client String
getOAuthUrl port = do
  (endpoint, credentials) <- RM.ask
  return $ mkUrl "https://accounts.google.com/o/oauth2/v2/auth"
    [ ( "response_type" , "code"               )
    , ( "client_id"     , clientId credentials )
    , ( "redirect_uri"  , baseURL port         )
    , ( "scope"         , scope endpoint       )
    ]

waitOAuth :: Port -> Client String
waitOAuth port = getOAuthUrl port >>= awaitCallback port

requestToken :: Int -> String -> Client ()
requestToken port personalToken = do
  credentials <- RM.asks snd
  STM.put NoCredentials
  creds <- post "https://www.googleapis.com/oauth2/v4/token"
    [ ( "code"          , personalToken            )
    , ( "client_id"     , clientId credentials     )
    , ( "client_secret" , clientSecret credentials )
    , ( "redirect_uri"  , baseURL port             )
    , ( "grant_type"    , "authorization_code"     )
    ] noBody
  STM.put creds

baseURL :: Port -> URL
baseURL port = "http://localhost:" ++ show port
