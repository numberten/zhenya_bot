module Bot.Component.Timer (
      timerComponent
   )  where

import Bot.Component
import Bot.Component.Stateful
import Control.Monad
import Control.Monad.State
import System.Time

         -- | `timerComponent` wraps the `StatefulComponent` with a specific state,
         -- |  as well as an explicit timer check used to propagate actions.
timerComponent :: (String 
               -> StateT (Integer,ClockTime) Bot ()) 
               -> Bot BotComponent
timerComponent action = stateful action' initialState
      where
            initialState = liftIO $ liftM ((,) 5) getClockTime
            action' message = do
                              (delay, lastFire)  <- get
                              now <- liftIO getClockTime
                              if predicate (timeSince now lastFire) delay
                                 then do
                                    lastFire <- liftIO getClockTime
                                    action message
                                    put (delay, lastFire)
                                 else do
                                    state <- get
                                    put state
                                 
         -- | Returns a string of the number of seconds between two times.  
            timeSince past present = timeDiffToString . diffClockTimes past $ present
         -- | Checks if `timeSince`s time is greater than a given delay.
            predicate dt d | dt == ""  = False
                           | otherwise = (>=d) . read . takeWhile (/=' ') $ dt

