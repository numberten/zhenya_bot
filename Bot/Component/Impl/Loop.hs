module Bot.Component.Impl.Loop (
    baconLoop
,   fizzBuzz
)   where

import Bot.Component
import Bot.Component.Combinator
import Bot.Component.Timer
import Bot.IO

import Control.Applicative
import Control.Monad.Trans.Identity

-- Component that `ircReply`s "bacon" every 5 seconds.
baconLoop :: Bot Component
baconLoop = timer action 5
  where
    action _ = liftBot $ ircReply "bacon"

-- | Demonstrates stacking multiple `ComponentPart`s using the `>>+` combinator.
fizzBuzz :: Bot Component
fizzBuzz = MkComponent  <$> timerP fizz 3 ()
                        >>+ timerP buzz 5
  where
    fizz :: String -> TimerT (IdentityT Bot) ()
    fizz _ = liftBot $ ircReply "fizz"

    buzz _ = liftBot $ ircReply "buzz"
