{-# LANGUAGE NamedFieldPuns #-}
module Bot.IO (
    ircWrite
,   ircWriteLoop
,   ircReply
,   ircReplyMaybe
,   ircReplyTo
,   onPrivMsg
,   onPrivMsgT
)   where

import Bot.Component

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.State
import Control.Monad.Trans.Identity
import Data.Time.Clock
import System.IO
import Text.Printf

-- | Reads a message from the queue and forwards to IRC.
-- Ensures that there is at minimum 1 / writeRate seconds between each
-- message.
ircWriteLoop :: TChan String -> Handle -> Double -> IO ()
ircWriteLoop chan handle writeRate = forever $ do
  let delay =  ceiling $ (1 / writeRate) * 1000000
  start     <- getCurrentTime
  message   <- atomically $ readTChan chan
  end       <- getCurrentTime
  let left  = delay - (ceiling $ diffUTCTime end start * 1000000)
  when (left > 0) (threadDelay left)
  liftIO $ hPutStr handle message

-- | Write a `String` message to IRC.
-- If the string doesn't fit in the 512 char length that IRC messages
-- are limited to, it will be broken up into separate messages.
ircWrite :: String -> String -> Bot ()
ircWrite command message = do
    nick    <-  gets botNick
    host    <-  gets botHost
    if command == "PRIVMSG"
        then    (sequence_  $ map writeToLoop
                            $ map (uncurry3 $ printf "%s %s%s\r\n")
                            $ partitionPrivMsg nick host channel message2)
            >>  (liftIO $ printf "> %s %s%s\n" command channel message2)
        else    writeToLoop (printf "%s %s\r\n" command message)
            >>  (liftIO $ printf "> %s %s\n" command message)
    where
        channel     = takeWhile (/= ':') message ++ ":"
        -- The text of the message minus the channel name and replacing all
        -- new lines with spaces.
        message2    =   map (\c -> if c == '\n' || c == '\r' then ' ' else c)
                    $   drop 1 $ dropWhile (/= ':') message
        uncurry3 f (a,b,c) = f a b c

        writeToLoop :: String -> Bot ()
        writeToLoop msg = do
            queue <- gets messageQueue
            liftIO $ atomically $ writeTChan queue msg

        partitionPrivMsg    ::  String -- nick
                            ->  String -- host
                            ->  String -- channel
                            ->  String -- message
                            ->  [(String,String,String)]
        partitionPrivMsg n h c m
            |   messageRoom >= lmessage =   [(command, c, m)]
            |   otherwise               =   (command, c, messageHead)
                                        :   partitionPrivMsg n h c messageTail
            where
                lnick       = length n
                lhost       = length h
                lchannel    = length c
                lmessage    = length m
                messageRoom = 510 - 11 - lnick - lchannel - lhost
                messageHead = take messageRoom m
                messageTail = drop messageRoom m


-- | If Just then send message to the current channel or nick.
ircReplyMaybe :: Maybe String -> Bot ()
ircReplyMaybe Nothing        = return ()
ircReplyMaybe (Just message) = ircReply message

-- | Send a message to the current channel or nick.
ircReply :: String -> Bot ()
ircReply message    =   gets currentChannel
                    >>= flip ircReplyTo message

-- | Send a message to a specific channel or nick.
ircReplyTo :: String -> String -> Bot ()
ircReplyTo channel message  = ircWrite "PRIVMSG" (channel ++ " :" ++ message)

onPrivMsg   ::  (String -> Bot ())
            ->  String -> Bot ()
onPrivMsg action = runIdentityT . onPrivMsgT (IdentityT . action)

-- | Filters out IRC messages that are not PRIVMSG's. In the event of a PRIVMSG,
-- the relevant part of the message is passed to the action function.
onPrivMsgT ::  (BotMonad b)
           =>  (String -> b ())
           ->  String -> b ()
onPrivMsgT action rawMessage =
    case words rawMessage of
        sender:"PRIVMSG":channel:message
            ->  do
                let currentNick     =   drop 1 $ takeWhile (/= '!') sender
                -- If this is a private message, we have to do a little bit of
                -- work to extract the nick of the person who just sent us the
                -- message.
                let currentChannel  = if head channel == '#'
                                        then channel
                                        else currentNick
                liftBot $ modify (\s -> s {currentChannel, currentNick})
                action (drop 1 $ unwords message)
        -- When catching the server's response to our WHO request, log
        -- our host and store it in state as botHost.
        _:number:nick:"*":botHost1:botHost2:_
            ->  do
                ourNick <- liftBot $ gets botNick
                let botHost = if (number == "352") && (nick == ourNick)
                                then botHost1 ++ ('@':botHost2)
                                else botHost
                liftBot $ modify (\s -> s {botHost})
        _   ->  return ()
