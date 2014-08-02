{-# LANGUAGE OverloadedStrings #-}

module Twitter.OAuth (login, myOAuth) where
import Data.ByteString.Char8 (ByteString, pack)
import Network.HTTP.Conduit
import System.Process (runCommand)
import Web.Authenticate.OAuth

myConsumerKey :: ByteString
myConsumerKey = "Insert key here"
myConsumerSecret :: ByteString
myConsumerSecret = "Insert secret here"

myOAuth :: OAuth
myOAuth = newOAuth { oauthServerName = "api.twitter.com"
                   , oauthConsumerKey = myConsumerKey
                   , oauthConsumerSecret = myConsumerSecret
                   , oauthRequestUri = "https://api.twitter.com/oauth/request_token"
                   , oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token"
                   , oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize"
                   }

getOAuthRequestToken :: IO Credential
getOAuthRequestToken = withManager $ \m -> getTemporaryCredential myOAuth m

authorizeUser :: Credential -> IO String
authorizeUser tempCredential = do
    let url = authorizeUrl myOAuth tempCredential
        browser = "/Applications/Google\\ Chrome.app/Contents/MacOS/Google\\ Chrome "
    runCommand $ browser ++ "\"" ++ url ++ "\"" -- To escape the '&'s
    putStr "Authorize the app and insert the code: "
    getLine

getOAuthAccessToken :: Credential -> String -> IO Credential
getOAuthAccessToken tempCredential pinCode = do
    let verifiedCred = injectVerifier (pack pinCode) tempCredential
    withManager $ \m -> getAccessToken myOAuth verifiedCred m

login :: IO Credential
login = do
    reqToken <- getOAuthRequestToken
    pinCode <- authorizeUser reqToken
    getOAuthAccessToken reqToken pinCode
