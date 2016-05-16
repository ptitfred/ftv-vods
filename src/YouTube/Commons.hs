{-# LANGUAGE OverloadedStrings #-}

module YouTube.Commons
    ( Credentials(..)
    , Parameter
    , Parameters
    , Token(..)
    , UserCredentials(..)
    , apiKey
    , clientId
    , clientSecret
    , getCredentials
    , noBody
    , mkUrl
    ) where

import Model (URL)

import Data.Aeson hiding (object)
import Data.Aeson.Types (typeMismatch)
import Data.List (intercalate)
import qualified Data.Text as T (unpack)
import System.Environment (getEnv)

type Parameters = [Parameter]
type Parameter = (String, String)
type ApiKey = String
type OAuthClientId = String
type OAuthSecret = String
type TokenType = String

data Credentials = Credentials ApiKey OAuthClientId OAuthSecret

data Token = Empty | Token String deriving (Eq)

instance Show Token where
  show  Empty    = ""
  show (Token t) = t

instance FromJSON Token where
  parseJSON (String s) = return $ mkToken $ T.unpack s
  parseJSON invalid = typeMismatch "Token" invalid

data UserCredentials = NoCredentials
                     | UserCredentials { userAccessToken  :: Token
                                       , userRefreshToken :: Token
                                       , userTokenType    :: TokenType
                                       } deriving (Show)

instance FromJSON UserCredentials where
  parseJSON (Object o) =
    UserCredentials <$> (Token <$> o .: "access_token")
                    <*> (Token <$> o .: "refresh_token")
                    <*> o .: "token_type"
  parseJSON invalid = typeMismatch "UserCredentials" invalid

mkToken :: String -> Token
mkToken "" = Empty
mkToken s  = Token s

getCredentials :: IO Credentials
getCredentials = Credentials <$> getEnv "API_KEY"
                             <*> getEnv "OAUTH_CLIENT_ID"
                             <*> getEnv "OAUTH_CLIENT_SECRET"

mkUrl :: URL -> Parameters -> URL
mkUrl url []         = url
mkUrl url parameters = url ++ "?" ++ toParameters parameters

toParameters :: Parameters -> String
toParameters = intercalate "&" . map toParameter

toParameter :: Parameter -> String
toParameter (a, b) = a ++ "=" ++ b

apiKey :: Credentials -> ApiKey
apiKey (Credentials key _ _) = key

clientId :: Credentials -> OAuthClientId
clientId (Credentials _ cid _) = cid

clientSecret :: Credentials -> OAuthSecret
clientSecret (Credentials _ _ secret) = secret

noBody :: Maybe ()
noBody = Nothing
