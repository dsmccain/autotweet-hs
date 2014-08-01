{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

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

mentions :: Credential -> IO (Either String [Tweet])
mentions credential = do
    request <- parseUrl $ baseUri ++ "/statuses/mentions_timeline.json?" ++
                 "contributor_details=true"
    response <- withManager $ \m -> do
        -- OAuth authentication
        signedRequest <- signOAuth myOAuth credential request
        -- Send request
        httpLbs signedRequest m
    return $ eitherDecode $ responseBody response

sendTweet :: Credential -> String -> IO (Either String Tweet)
sendTweet credential message = do
    request <- parseUrl $ baseUri ++ "/statuses/update.json"
    let update = pack message
        postData = [("status", update)] :: [(ByteString, ByteString)]
        request' = urlEncodedBody postData request
    response <- withManager $ \m -> do
        -- OAuth authentication
        signedRequest <- signOAuth myOAuth credential request'
        -- Send request
        httpLbs signedRequest m
    return $ eitherDecode $ responseBody response

tweetInfo :: Credential
          -> Int    -- ID of the tweet the information is wanted about
          -> IO (Either String Tweet)
tweetInfo credential tweetID = do
    request <- parseUrl $ baseUri ++ "/statuses/show.json?id=" ++ show tweetID
    response <- withManager $ \m -> do
        -- OAuth authentication
        signedRequest <- signOAuth myOAuth credential request
        -- Send request
        httpLbs signedRequest m
    return $ eitherDecode $ responseBody response

replyTweet :: Credential
           -> Int    -- Tweet ID that the reply is directed to
           -> String -- Message to send as a reply
           -> IO (Either String Tweet)
replyTweet credential tweetID message = do
    info <- tweetInfo credential tweetID
    case info of
         Right info' -> do
             request <- parseUrl $ baseUri ++ "/statuses/update.json"
             let username = (unpack . screen_name . user) info'
                 update = pack $ '@' : username ++ " " ++ message
                 postData = [ ("status", update)
                            , ("in_reply_to_status_id", pack $ show tweetID)
                            ] :: [(ByteString, ByteString)]
                 request' = urlEncodedBody postData request
             response <- withManager $ \m -> do
                 -- OAuth authentication
                 signedRequest <- signOAuth myOAuth credential request'
                 -- Send request
                 httpLbs signedRequest m
             return $ eitherDecode $ responseBody response
         _ -> return info

main :: IO ()
main = do
    accessToken <- login

    eMentions <- mentions accessToken
    -- If Left, print the error. If Right, print the wanted information on screen
    case eMentions of
         Left err -> do
             putStrLn "There was an error when getting the mentions"
             putStrLn err
         Right m  -> do
             let mLatest = listToMaybe m -- Last person to mention the user
             case mLatest of
                  Nothing -> putStrLn "No one has mentioned me!"
                  Just tweet -> do
                      let username = (unpack . screen_name . user) tweet
                          tweetID = Main.id tweet
                          msg = "this is another autoreply!!"
                      putStrLn $ "Sending a reply to " ++ username ++ "!"
                      eReply <- replyTweet accessToken tweetID msg
                      print eReply

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
