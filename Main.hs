import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Maybe (listToMaybe)
import Data.Text (unpack)
import Data.Time.Clock (getCurrentTime)
import Twitter.Action
import Twitter.OAuth
import Web.Authenticate.OAuth (Credential)

-- Autorespond to the last person that mentioned the user
-- Tweets if that hasn't happened
autorespondToFirstMention :: ReaderT Credential IO ()
autorespondToFirstMention = do
    eMentions <- mentions
    case eMentions of
         Left err -> do
             liftIO $ putStrLn "There was an error when getting the mentions"
             liftIO $ putStrLn err
         Right m  -> do
             let mLatest = listToMaybe m -- Last person to mention the user
             case mLatest of
                  Nothing -> do
                      liftIO $ putStrLn $ "No one has mentioned me! " ++
                          "Gotta let the world know"
                      eTweet <- sendTweet "No one has mentioned me!!"
                      liftIO $ print eTweet
                  Just tweet -> do
                      currentTime <- liftIO getCurrentTime
                      let username = (unpack . screen_name . user) tweet
                          tweetID = Twitter.Action.id tweet
                          msg = "oh yeah? " ++ show currentTime ++ "!"
                      liftIO $ putStrLn $ "Replying to " ++ username ++ "!"
                      eReply <- replyTweet tweetID msg
                      liftIO $ print eReply

main :: IO ()
main = login >>= \accessToken -> flip runReaderT accessToken $ do
    friends <- friendList "McCain"
    liftIO $ putStrLn "My friend list:"
    liftIO $ print friends
    followers <- followerList "McCain"
    liftIO $ putStrLn "My followers:"
    liftIO $ print followers
    liftIO $ putStrLn "Time to tweet!"
    autorespondToFirstMention
