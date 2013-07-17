{-# LANGUAGE NamedFieldPuns #-}
module Bot.IO (
    ircWrite
,   ircReply
,   ircReplyTo
,   onPrivMsg
,   onPrivMsgT
)   where

import Bot.Component

import Control.Monad.State
import Control.Monad.Trans.Identity
import Text.Printf

-- | Write a `String` message to IRC.
ircWrite :: String -> String -> Bot ()
ircWrite command message = do
	handle  <-  gets socket
	liftIO  $   hPrintf handle "%s %s\r\n" command message
	        >>  printf "> %s %s\n" command message

-- | Send a message to the current channel or nick.
ircReply :: String -> Bot ()
ircReply message    =   gets currentChannel 
                    >>= flip ircReplyTo message

-- | Send a message to a specific channel or nick.
ircReplyTo :: String -> String -> Bot ()
ircReplyTo channel message = ircWrite "PRIVMSG" (channel ++ " :" ++ message)

-- | Executes the given action on the message portion of PRIVMSG's and do
-- nothing for other types of IRC messages. 
onPrivMsg :: (String -> Bot ()) -> String -> Bot ()
onPrivMsg action = runIdentityT . onPrivMsgT actionT
    where
        actionT :: String -> IdentityT Bot ()
        actionT = lift . action

-- | Filters out IRC messages that are not PRIVMSG's. In the event of a PRIVMSG,
-- the relevant part of the message is passed to the action function.
onPrivMsgT  ::  (MonadTrans t, Monad (t Bot)) 
            =>  (String -> t Bot ()) 
            ->  String ->  t Bot ()
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
                lift $ modify (\s -> s {currentChannel, currentNick})
                action (drop 1 $ unwords message)
        _   ->  return ()

