{-# LANGUAGE RecordWildCards #-}
module Bot.Component.Impl.Loop (
    baconLoop
)   where

import Bot.Component
import Bot.Component.Command
import Bot.Component.Timer
import Bot.IO

import Control.Monad.State
import System.Time

-- Component that `ircReply`s "bacon" every 5 seconds.
baconLoop :: Bot BotComponent
baconLoop = timerComponent action initialState
   where
      initialState = liftIO $ getClockTime >>= return . (,) 5
      action = simpleCommandT "" $ lift $ ircReply "bacon"
