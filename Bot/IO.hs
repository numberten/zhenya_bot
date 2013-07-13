{-# LANGUAGE NamedFieldPuns #-}
module Bot.IO (
    ircWrite
,   ircReply
,   ircReplyTo
,   onPrivMsg
)   where

import Bot.Component

import Control.Monad.State
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

-- | Filters out IRC messages that are not PRIVMSG's. In the event of a PRIVMSG,
-- the relevant part of the message is passed to the action function.
onPrivMsg :: String -> (String -> Bot ()) -> Bot ()
onPrivMsg rawMessage action = 
    case words rawMessage of
        sender:"PRIVMSG":channel:message
            ->  do
                -- If this is a private message, we have to do a little bit of
                -- work to extract the nick of the person who just sent us the
                -- message.
                let currentChannel = if head channel == '#'
                                        then channel 
                                        else drop 1 $ takeWhile (/= '!') sender
                modify (\s -> s { currentChannel  })
                action (drop 1 $ unwords message)
        _   ->  return ()

