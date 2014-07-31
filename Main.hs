{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Aeson
import Data.ByteString.Char8 (ByteString, pack)
import Data.Maybe (listToMaybe)
import Data.Text (append, unpack, Text)
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

mentions :: Credential -> IO (Either String [Tweet])
mentions credential = do
    request <- parseUrl $
        "https://api.twitter.com/1.1/statuses/mentions_timeline.json?" ++
                 "contributor_details=true"
    response <- withManager $ \m -> do
        -- OAuth authentication
        signedRequest <- signOAuth myOAuth credential request
        -- Send request
        httpLbs signedRequest m
    return $ eitherDecode $ responseBody response

tweet :: Credential -> String -> IO (Either String Tweet)
tweet credential message = do
    request <- parseUrl "https://api.twitter.com/1.1/statuses/update.json"
    let update = pack message
        postData = [("status", update)] :: [(ByteString, ByteString)]
        request' = urlEncodedBody postData request
    response <- withManager $ \m -> do
        -- OAuth authentication
        signedRequest <- signOAuth myOAuth credential request'
        -- Send request
        httpLbs signedRequest m
    return $ eitherDecode $ responseBody response

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
             putStr "Latest tweet was by "
             let mLatest = listToMaybe m
                 mUser = maybe "no one?!" ((`append` "!") . screen_name . user) mLatest
             putStrLn $ unpack mUser
             -- eTweet <- tweet accessToken "This is a test! Sorry for the spamming! :D"
             -- case eTweet of
             --      Left err -> do
             --          putStrLn "There was an error when tweeting:"
             --          putStrLn err
             --      Right t -> do
             --          putStrLn "Sending tweet..."
             --          print t

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
