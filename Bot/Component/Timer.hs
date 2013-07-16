{-# LANGUAGE RecordWildCards #-}
module Bot.Component.Timer (
      TimerComponent(..)
   )  where

import Bot.Component
import Control.Monad.State
import System.Time

data TimerComponent = TimerComponent {
         lastFire :: ClockTime      --Time that action was last fired.
      ,  delay    :: Integer        --Delay between actions, in seconds.
      ,  state    :: StateT ClockTime Bot ()
}

instance Botable TimerComponent where
      process _ TimerComponent{..} = action
         where
            action :: Bot TimerComponent
            action   = do
                           now <- liftIO getClockTime
                           if predicate (timeDiffStr now lastFire) delay
                              then do
                                 lastFire <- liftIO getClockTime
                                 execStateT state now
                                 return TimerComponent{..}
                              else return TimerComponent{..}
            timeDiffStr t1 t2 = timeDiffToString . diffClockTimes t1 $ t2
            predicate "" _ = False
            predicate dt d = (read . takeWhile (/=' ') $ dt) >= d

