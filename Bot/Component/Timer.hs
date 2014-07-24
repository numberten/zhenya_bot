module Bot.Component.Timer (
    TimerT
,   ioTimer
,   timer
,   timerP
)  where

import Bot.Component
import Bot.Component.Stateful

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Identity
import Prelude hiding (catch)
import System.Time

type TimerT = StateT ClockTime

-- | Creates an IO thread that handles errors gracefully and that can be `>>`'ed
-- to a resulting `Bot` value to create the timer on the creation of the
-- component.
ioTimer ::  String -- name used to identify the thread in error messages
        ->  Int -- the delay in milliseconds
        ->  IO () -- the IO action to perform
        ->  Bot () -- A Bot action that will kick off the timer
ioTimer name delay action = void $ liftIO $ forkIO loop
    where
        loop = do
            action `catch` handler
            threadDelay delay
            loop

        handler :: SomeException -> IO ()
        --handler = void . return
        handler = putStrLn . (("ERROR: " ++ name ++ " Error: ") ++) . show

-- | A `timer` wraps the `StateT` with a specific state, as well as an explicit
-- timer check used to propagate actions.
timer   :: (String -> TimerT (IdentityT Bot) ())
        ->  Integer
        ->  Bot Component
timer action delay = MkComponent <$> timerP action delay () <*> return Nothing

-- | A more generic `timer` that creates a `ComponentPart` which allows it to be
-- stacked with the `>>+` combinator.
timerP  ::  BotMonad b
        =>  (String -> TimerT b ())
        ->  Integer
        ->  BotExtractor b -> Bot (ComponentPart (TimerT b))
timerP action delay = statefulP action' initialState
  where
    initialState = liftIO getClockTime
    action' message = do
        lastFire  <- get
        now <- liftBot $ liftIO getClockTime
        when (predicate (timeSince now lastFire) delay) $ do
            lastFire <- liftBot $ liftIO getClockTime
            action message
            put lastFire

    -- Returns a string of the number of seconds between two times.
    timeSince past present = timeDiffToString . diffClockTimes past $ present
    -- Checks if `timeSince`s time is greater than a given delay.
    predicate dt d | dt == ""  = False
                   | otherwise = (>=d) . read . takeWhile (/=' ') $ dt

