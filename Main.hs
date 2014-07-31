{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Aeson
import Data.ByteString.Char8 (ByteString, pack)
import Data.Maybe (listToMaybe)
import Data.Text (append, unpack, Text)
import GHC.Generics
import Network.HTTP.Conduit
import System.Process (runCommand)
import Web.Authenticate.OAuth

myConsumerKey :: ByteString
myConsumerKey = "Insert key here"
myConsumerSecret :: ByteString
myConsumerSecret = "Insert secret here"

myOAuth :: OAuth
myOAuth = newOAuth { oauthServerName     = "api.twitter.com"
                   , oauthConsumerKey    = myConsumerKey
                   , oauthConsumerSecret = myConsumerSecret
                   , oauthRequestUri = "https://api.twitter.com/oauth/request_token"
                   , oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token"
                   , oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize"
                   }

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

-- getOAuthRequestToken -- gives requestTokenResponse
getOAuthRequestToken :: IO Credential
getOAuthRequestToken = withManager $ \m -> getTemporaryCredential myOAuth m

-- authorizeUser -- uses requestTokenResponse, gives oauthVerifier
authorizeUser :: Credential -> IO String
authorizeUser tempCredential = do
    let url = authorizeUrl myOAuth tempCredential
        browser = "/Applications/Google\\ Chrome.app/Contents/MacOS/Google\\ Chrome "
    runCommand $ browser ++ "\"" ++ url ++ "\"" -- To escape the "&"
    putStr "Authorize the app and insert the code: "
    getLine

-- getOAuthAccessToken -- uses requestTokenResponse and oauthVerifier, gives token
getOAuthAccessToken :: Credential -> String -> IO Credential
getOAuthAccessToken tempCredential pinCode = do
    let verifiedCred = injectVerifier (pack pinCode) tempCredential
    withManager $ \m -> getAccessToken myOAuth verifiedCred m

mentions :: Credential -> String -> IO (Either String [Tweet])
mentions credential name = do
    request <- parseUrl $
        "https://api.twitter.com/1.1/statuses/mentions_timeline.json?" ++
                 "screen_name=" ++ name ++ "&contributor_details=true"
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
    -- Login:
    reqToken <- getOAuthRequestToken
    pinCode <- authorizeUser reqToken
    accessToken <- getOAuthAccessToken reqToken pinCode

    eMentions <- mentions accessToken "mccain"
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
