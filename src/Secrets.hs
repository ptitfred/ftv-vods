{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Secrets
Description : Routines to safely store passwords or tokens.
Copyright   : (c) Frédéric Menou, 2016
License     : MIT License
Maintainer  : Frédéric Menou <frederic.menou@gmail.com>
Stability   : experimental
Portability : X11/Freedesktop

Store a password:
> password <- getLine -- read from stdin, popup, ...
> let label = "Fancy label to be displayed in Keyring"
> let attributes = [ ("application", "my-application") ]
> success <- storePassword label password attributes
> if success
> then putStrLn "Password saved!"
> else putStrLn "Failure"

Retrieve passwords:
> let filters = [ ("application", "my-application") ]
> Just password <- findPassword filters

-}
module Secrets
    (
    -- * Types
      Attributes
    , Filter
    , Label
    , Password
    -- * API
    , findPassword
    , findPasswords
    , storePassword
    ) where

import           Control.Concurrent.MVar      (MVar, newEmptyMVar, takeMVar, putMVar)
import           Control.Monad                (void)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8   as C (pack)
import           Data.List                    (uncons)
import           Data.Maybe                   (fromJust)
import qualified Data.Map.Lazy           as M (Map, elems, fromList)
import           DBus
import           DBus.Client

type Label      = String
type Password   = ByteString
type Attributes = [(String, String)]
type Filter     = [(String, String)]
type Secret     = (ObjectPath, ByteString, ByteString, String)

data Success    = Success Variant
                | PromptNeeded ObjectPath
                  deriving (Show)

findSecrets :: Filter -> IO [Secret]
findSecrets filters = do
  client  <- connectSession
  paths   <- searchItems client filters
  session <- openSession client
  getSecrets client paths session

findPassword :: Filter -> IO (Maybe ByteString)
findPassword filters = fmap fst . uncons <$> findPasswords filters

findPasswords :: Filter -> IO [ByteString]
findPasswords filters = map readPassword <$> findSecrets filters

storePassword :: Label -> Password -> Attributes -> IO Bool
storePassword label password attributes = do
  client  <- connectSession
  session <- openSession client

  unlockSuccess <- unlockPasswords client defaultCollection
  case unlockSuccess of
    Success _ -> do
      let properties = mkProperties label attributes
      let secret = mkPlainSecret session password

      reply <- call_ client (methodCall defaultCollection "org.freedesktop.Secret.Collection" "CreateItem")
        { methodCallDestination = Just "org.freedesktop.secrets"
        , methodCallBody = [ toVariant properties
                           , toVariant secret
                           , toVariant True
                           ]
        }
      result <- promptIfRequired client reply
      case result of
        Success _ -> return True
        _         -> return False
    _ -> return False

defaultCollection :: ObjectPath
defaultCollection = "/org/freedesktop/secrets/aliases/default"

readPassword :: Secret -> ByteString
readPassword (_, _, password, _) = password

mkProperties :: String -> Attributes -> M.Map String Variant
mkProperties label attributes =
  M.fromList [ ("org.freedesktop.Secret.Item.Label",      toVariant label)
             , ("org.freedesktop.Secret.Item.Attributes", toVariant (M.fromList attributes))
             ]

mkPlainSecret :: ObjectPath -> ByteString -> Secret
mkPlainSecret session password = (session, C.pack "", password, "text/plain")

promptIfRequired :: Client -> MethodReturn -> IO Success
promptIfRequired client reply = do
  let result = methodReturnBody reply
  let promptPath = fromJust . fromVariant $ result !! 1
  askPrompt client $ analyze (result!!0) promptPath

unlockPasswords :: Client -> ObjectPath -> IO Success
unlockPasswords client collection = do
  reply <- call_ client (methodCall "/org/freedesktop/secrets" "org.freedesktop.Secret.Service" "Unlock")
    { methodCallDestination = Just "org.freedesktop.secrets"
    , methodCallBody = [ toVariant [collection] ]
    }
  promptIfRequired client reply

askPrompt :: Client -> Success -> IO Success
askPrompt _       s@(Success _)               = return s
askPrompt client pn@(PromptNeeded path) = do
  openPrompt client path
  callback <- waitPrompt client path
  if (fromJust . fromVariant $ callback !! 0)
  then putStrLn "Dismissed, aborting" >> return pn
  else putStrLn "Accepted" >> return (Success $ callback !! 1)

openPrompt :: Client -> ObjectPath -> IO ()
openPrompt client path =
  void $ call_ client (methodCall path "org.freedesktop.Secret.Prompt" "Prompt")
    { methodCallDestination = Just "org.freedesktop.secrets"
    , methodCallBody = [ toVariant ("0" :: String) ]
    }

waitPrompt :: Client -> ObjectPath -> IO [Variant]
waitPrompt client path = do
  let matchRule = matchAny { matchPath = Just path }
  resultHolder <- newEmptyMVar
  void $ addMatch client matchRule (promptCompletedHandler resultHolder)
  takeMVar resultHolder

promptCompletedHandler :: MVar [Variant] -> Signal -> IO ()
promptCompletedHandler barrier = putMVar barrier . signalBody

analyze :: Variant -> ObjectPath -> Success
analyze v "/"    = Success v
analyze _ prompt = PromptNeeded prompt

searchItems :: Client -> Filter -> IO [ObjectPath]
searchItems client filters = do
  reply <- call_ client (methodCall "/org/freedesktop/secrets" "org.freedesktop.Secret.Service" "SearchItems")
    { methodCallDestination = Just "org.freedesktop.secrets"
    , methodCallBody = [toVariant (M.fromList filters)]
    }
  return $ fromJust $ fromVariant $ methodReturnBody reply !! 0

openSession :: Client -> IO ObjectPath
openSession client = do
  reply <- call_ client (methodCall "/org/freedesktop/secrets" "org.freedesktop.Secret.Service" "OpenSession")
    { methodCallDestination = Just "org.freedesktop.secrets"
    , methodCallBody = [toVariant ("plain" :: String), toVariant (toVariant ("" :: String))]
    }
  return $ fromJust $ fromVariant $ methodReturnBody reply !! 1

getSecrets :: Client -> [ObjectPath] -> ObjectPath -> IO [Secret]
getSecrets client paths session = do
  reply <- call_ client (methodCall "/org/freedesktop/secrets" "org.freedesktop.Secret.Service" "GetSecrets")
    { methodCallDestination = Just "org.freedesktop.secrets"
    , methodCallBody = [toVariant paths, toVariant session]
    }
  return $ M.elems $ fromJust $ (fromVariant (methodReturnBody reply !! 0) :: Maybe (M.Map ObjectPath Secret))
