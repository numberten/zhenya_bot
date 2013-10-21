module Bot.Component.Timer (
    TimerT
,   timer
,   timerP
)  where

import Bot.Component
import Bot.Component.Stateful

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans.Identity
import System.Time

type TimerT = StateT ClockTime

-- | A `timer` wraps the `StateT` with a specific state, as well as an explicit
-- timer check used to propagate actions.
timer   :: (String -> TimerT (IdentityT Bot) ())
        ->  Integer
        ->  Bot Component
timer action delay = MkComponent <$> timerP action delay ()

-- | A more generic `timer` that creates a `ComponentPart` which allows it to be
-- stacked with the `>>+` combinator.
timerP  ::  BotMonad b
        =>  (String -> TimerT b ())
        ->  Integer
        ->  BotExtractor b -> Bot (ComponentPart (TimerT b))
timerP action delay = statefulP action' initialState
  where
    initialState = liftIO $ getClockTime
    action' message = do
                      lastFire  <- get
                      now <- liftBot $ liftIO getClockTime
                      if predicate (timeSince now lastFire) delay
                        then do
                            lastFire <- liftBot $ liftIO getClockTime
                            action message
                            put lastFire
                        else return ()

    -- Returns a string of the number of seconds between two times.
    timeSince past present = timeDiffToString . diffClockTimes past $ present
    -- Checks if `timeSince`s time is greater than a given delay.
    predicate dt d | dt == ""  = False
                   | otherwise = (>=d) . read . takeWhile (/=' ') $ dt

