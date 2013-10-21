module Bot.Component.Impl.Loop (
    baconLoop
)   where

import Bot.Component
import Bot.Component.Timer
import Bot.IO

import Control.Monad.State

-- Component that `ircReply`s "bacon" every 5 seconds.
baconLoop :: Bot Component
baconLoop = timerComponent action 5
  where
    action _ = liftBot $ ircReply "bacon"
