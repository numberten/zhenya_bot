{-# LANGUAGE RecordWildCards #-}
module Bot.Component.Impl.Loop (
    baconLoop
)   where

import Bot.Component
import Bot.Component.Command
import Bot.Component.Timer
import Bot.IO
import Debug.Trace

import Control.Monad.State
import System.Time

baconLoop :: Bot BotComponent
baconLoop = do
               initialState <- liftIO getClockTime
               delay'       <- return 5
               mkComponent TimerComponent { lastFire = initialState
                                          , delay    = delay'
                                          , state    = (simpleCommandT "" $ lift $ ircReply "bacon") ""
                                          }

