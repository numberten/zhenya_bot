module Bot.Component.Impl.Loop (
    baconLoop
)   where

import Bot.Component
import Bot.Component.Command
import Bot.Component.Timer
import Bot.IO

import Control.Monad.State

-- Component that `ircReply`s "bacon" every 5 seconds.
baconLoop :: Bot BotComponent
baconLoop = timerComponent action 5
  where
    action = simpleCommandT "" $ lift $ ircReply "bacon"
