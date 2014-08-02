{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleContexts #-}

module Twitter.Action where
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson
import Data.ByteString.Char8 (ByteString, pack)
import Data.Text (unpack, Text)
import GHC.Generics
import Network.HTTP.Conduit
import Twitter.OAuth
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

twitterPOSTreq :: (FromJSON a, MonadThrow m, MonadIO m, MonadBaseControl IO m) =>
                  String -> [(ByteString, ByteString)] ->
                  ReaderT Credential m (Either String a)
twitterPOSTreq req postData = do
    credential <- ask
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
                 String -> ReaderT Credential m (Either String a)
twitterGETreq req = twitterPOSTreq req []

mentions :: ReaderT Credential IO (Either String [Tweet])
mentions = twitterGETreq request
  where request = "/statuses/mentions_timeline.json?contributor_details=true"

sendTweet :: String -> ReaderT Credential IO (Either String Tweet)
sendTweet message = twitterPOSTreq request postData
  where postData = [("status", pack message)] :: [(ByteString, ByteString)]
        request = "/statuses/update.json"

tweetInfo :: Int -- ID of the tweet the information is wanted about
          -> ReaderT Credential IO (Either String Tweet)
tweetInfo tweetID = twitterGETreq request
  where request = "/statuses/show.json?id=" ++ show tweetID

replyTweet :: Int    -- Tweet ID that the reply is directed to
           -> String -- Message to send as a reply
           -> ReaderT Credential IO (Either String Tweet)
replyTweet tweetID message = do
    info <- tweetInfo tweetID
    case info of
         Right info' -> do
             let request = "/statuses/update.json"
                 username = (unpack . screen_name . user) info'
                 update = pack $ '@' : username ++ " " ++ message
                 postData = [ ("status", update)
                            , ("in_reply_to_status_id", pack $ show tweetID)
                            ] :: [(ByteString, ByteString)]
             twitterPOSTreq request postData
         _ -> return info

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
