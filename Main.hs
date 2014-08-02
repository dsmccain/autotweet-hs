import Data.Maybe (listToMaybe)
import Data.Text (unpack)
import Twitter.Action
import Twitter.OAuth

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
                          tweetID = Twitter.Action.id tweet
                          msg = "oh yeah?!"
                      putStrLn $ "Sending a reply to " ++ username ++ "!"
                      eReply <- replyTweet accessToken tweetID msg
                      print eReply
