{-# LANGUAGE OverloadedStrings #-}

module YouTube.OAuth
    ( Port
    , oauthAuthorizeUrl
    , oauthCredentials
    ) where

import           Model (URL)
import           YouTube.Commons
import qualified YouTube.Client as YT

import           Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as T
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Web.Scotty as S (ActionM, get, param, redirect, scottyApp, html)

type Port = Int

oauthAuthorizeUrl :: Port -> URL
oauthAuthorizeUrl port = baseURL port ++ "/authorize"

oauthCredentials :: Port -> IO UserCredentials
oauthCredentials port = waitOAuth port >>= requestToken port

application :: URL -> MVar String -> IO Wai.Application
application authUrl returnValue = S.scottyApp $ do
  S.get "/authorize" (shortenerHandler authUrl)
  S.get "/" (callbackHandler returnValue)

shortenerHandler :: String -> S.ActionM ()
shortenerHandler url = S.redirect (T.pack url)

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
  YT.post noBody NoCredentials $ mkUrl "POST https://www.googleapis.com/oauth2/v4/token"
    [ ( "code"          , personalToken            )
    , ( "client_id"     , clientId credentials     )
    , ( "client_secret" , clientSecret credentials )
    , ( "redirect_uri"  , baseURL port             )
    , ( "grant_type"    , "authorization_code"     )
    ]

baseURL :: Port -> URL
baseURL port = "http://localhost:" ++ show port
