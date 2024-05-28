module Main where

import Control.Concurrent
import Control.Monad
import System.Random
import Data.List (delete)

-- | Represents a user in the social network.
--   Each user has a username and a mailbox to receive messages.
data User = User {
    userName :: String,
    userMailbox :: MVar [Message]
} deriving (Eq)

-- | Represents a message in the social network.
newtype Message = Message String deriving (Eq, Show)

-- | Create a new user with an empty mailbox.
createUser :: String -> IO User
createUser name = do
    mailbox <- newMVar []
    return $ User name mailbox

-- | Send a message from one user to another.
sendMessage :: User -> User -> Message -> IO ()
sendMessage fromUser toUser message = do
    mailbox <- takeMVar (userMailbox toUser)
    let newMailbox = message : mailbox
    putMVar (userMailbox toUser) newMailbox
    putStrLn $ userName fromUser ++ " sent to " ++ userName toUser ++ ": " ++ show message

-- | User's action in the social network.
userAction :: [User] -> User -> MVar Int -> IO ()
userAction users currentUser messageCount = forever $ do
    count <- takeMVar messageCount
    when (count >= 100) $ do
        putStrLn "Reached 100 messages, stopping."
        return ()

    targetUser <- randomSelect $ delete currentUser users
    randomMsg <- fmap Message $ randomSelect ["whatsup","Good day","Good morning","Hi there", "Hello!", "How are you?", "Nice day!", "Greetings"]
    sendMessage currentUser targetUser randomMsg

    putMVar messageCount (count + 1)
    randomDelay <- randomRIO (1000000, 2500000) -- Random delay between 1 and 2.5 seconds
    threadDelay randomDelay

-- | Randomly select an item from a list.
randomSelect :: [a] -> IO a
randomSelect lst = do
    index <- randomRIO (0, length lst - 1)
    return $ lst !! index

-- | Main function to start the social network simulation.
main :: IO ()
main = do
    messageCount <- newMVar 0

    -- Creating 10 users
    users <- mapM createUser ["Apoorva", "Bunny", "Charlie", "Dia", "Edward", 
                              "Faran", "Ganesh", "Harsh", "Ian", "Jia"]

    -- Fork a thread for each user to simulate actions
    mapM_ (\user -> forkIO $ userAction users user messageCount) users

    -- Wait for the simulation to end
    _ <- getLine
    putStrLn "Simulation has ended."

