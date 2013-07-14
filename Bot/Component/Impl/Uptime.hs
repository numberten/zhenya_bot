module Bot.Component.Impl.Uptime (
    uptime
)   where

import Bot.Component
import Bot.Component.Command
import Bot.Component.Stateful
import Bot.IO
import Bot.Time

import Control.Monad.State
import System.Time

-- | Report the IRC bot's uptime upon encountering the "!uptime" command.
uptime :: Bot BotComponent
uptime = stateful commandAction initialState
    where
        initialState = liftIO getClockTime

        commandAction = simpleCommandT "!uptime" $ do
            now     <- liftIO getClockTime
            zero    <- get
            lift $ ircReply $ pretty $ diffClockTimes now zero
