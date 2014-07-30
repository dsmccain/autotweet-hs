{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Conduit
import Web.Authenticate.OAuth

myConsumerKey :: ByteString
myConsumerKey = "Insert key here"
myConsumerSecret :: ByteString
myConsumerSecret = "Insert secret here"

myOAuth :: OAuth
myOAuth = newOAuth { oauthServerName     = "api.twitter.com"
                   , oauthConsumerKey    = myConsumerKey
                   , oauthConsumerSecret = myConsumerSecret
                   }

myAccessToken :: ByteString
myAccessToken = "Insert token here"
myAccessTokenSecret :: ByteString
myAccessTokenSecret = "Insert secret here"

myCred :: Credential
myCred = newCredential myAccessToken myAccessTokenSecret

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

mentions :: String -> IO (Either String [Tweet])
mentions name = do
    request <- parseUrl $
        "https://api.twitter.com/1.1/statuses/mentions_timeline.json?" ++
                 "screen_name=" ++ name ++ "&contributor_details=true"
    response <- withManager $ \m -> do
        -- OAuth authentication
        signedRequest <- signOAuth myOAuth myCred request
        -- Send request
        httpLbs signedRequest m
    return $ eitherDecode $ responseBody response


main :: IO ()
main = do
    eMentions <- mentions "mccain"
    -- If Left, print the error. If Right, print the wanted information on screen
    either putStrLn (mapM_ print) eMentions

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
