{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleContexts #-}

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson
import Data.ByteString.Char8 (ByteString, pack)
import Data.Maybe (listToMaybe)
import Data.Text (unpack, Text)
import GHC.Generics
import Network.HTTP.Conduit
import OAuth.Twitter
import Web.Authenticate.OAuth

-- Information we want about the user that is tweeting the message
data User = User { screen_name :: !Text
                 } deriving (Show, Generic)

-- Information about the user's tweet
data Tweet = Tweet { user :: User
                   , text :: !Text
                   , id :: !Int
                   } deriving (Show, Generic)

instance FromJSON User
instance ToJSON User
instance FromJSON Tweet
instance ToJSON Tweet

baseUri :: String
baseUri = "https://api.twitter.com/1.1"

{-
    The type constraints for twitterGETreq and twitterPOSTreq come from
    the following functions:
        parseUrl :: MonadThrow m => String -> m Request
        withManager :: (MonadIO m, MonadBaseControl IO m) =>
                       (Manager -> ResourceT m a) -> m a
        eitherDecode :: FromJSON a => ByteString -> Either String a
-}
twitterPOSTreq :: (FromJSON a, MonadThrow m, MonadIO m, MonadBaseControl IO m) =>
                  Credential -> String ->
                  [(ByteString, ByteString)] -> m (Either String a)
twitterPOSTreq credential req postData = do
    request <- parseUrl $ baseUri ++ req
    -- Change the request method to POST if postData is not empty
    let request' = if null postData
                   then request
                   else urlEncodedBody postData request
    response <- withManager $ \m -> do
        -- OAuth authentication
        signedRequest <- signOAuth myOAuth credential request'
        -- Send request
        httpLbs signedRequest m
    return $ eitherDecode $ responseBody response

twitterGETreq :: (FromJSON a, MonadThrow m, MonadIO m, MonadBaseControl IO m) =>
                 Credential -> String -> m (Either String a)
twitterGETreq credential req = twitterPOSTreq credential req []

mentions :: Credential -> IO (Either String [Tweet])
mentions credential = twitterGETreq credential request
  where request = "/statuses/mentions_timeline.json?contributor_details=true"

sendTweet :: Credential -> String -> IO (Either String Tweet)
sendTweet credential message = twitterPOSTreq credential request postData
  where
      postData = [("status", pack message)] :: [(ByteString, ByteString)]
      request = "/statuses/update.json"

tweetInfo :: Credential
          -> Int    -- ID of the tweet the information is wanted about
          -> IO (Either String Tweet)
tweetInfo credential tweetID = twitterGETreq credential request
  where request = "/statuses/show.json?id=" ++ show tweetID

replyTweet :: Credential
           -> Int    -- Tweet ID that the reply is directed to
           -> String -- Message to send as a reply
           -> IO (Either String Tweet)
replyTweet credential tweetID message = do
    info <- tweetInfo credential tweetID
    case info of
         Right info' -> do
             let request = "/statuses/update.json"
                 username = (unpack . screen_name . user) info'
                 update = pack $ '@' : username ++ " " ++ message
                 postData = [ ("status", update)
                            , ("in_reply_to_status_id", pack $ show tweetID)
                            ] :: [(ByteString, ByteString)]
             twitterPOSTreq credential request postData
         _ -> return info

main :: IO ()
main = do
    accessToken <- login

    eMentions <- mentions accessToken
    case eMentions of
         Left err -> do
             putStrLn "There was an error when getting the mentions"
             putStrLn err
         Right m  -> do
             let mLatest = listToMaybe m -- Last person to mention the user
             case mLatest of
                  Nothing -> do
                      putStrLn "No one has mentioned me! Gotta let the world know"
                      eTweet <- sendTweet accessToken "No one has mentioned me!!"
                      print eTweet
                  Just tweet -> do
                      let username = (unpack . screen_name . user) tweet
                          tweetID = Main.id tweet
                          msg = "oh yeah?!"
                      putStrLn $ "Sending a reply to " ++ username ++ "!"
                      eReply <- replyTweet accessToken tweetID msg
                      print eReply

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
