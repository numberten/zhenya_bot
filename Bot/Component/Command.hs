module Bot.Component.Command (
    simpleCommand
,   simpleCommandT
,   emptyCommand
,   command
,   commandT
,   UsageMessage (..)
)   where

import Bot.Component
import Bot.IO

import Control.Monad.Trans
import Control.Monad.Trans.Identity

-- | A Maybe [String] datatype with semantically clear value constructors
-- for use as usage messages.
data UsageMessage = UsageMessage [String] | NoUsageMessage

-- | A convenience function that wraps `command` for commands that don't need to
-- take arguments.
simpleCommand   ::  String
                ->  Bot ()
                ->  Bot Component
simpleCommand trigger action = command NoUsageMessage trigger (const action)

-- | Create a command `BotComponent` for a command that requires arguments.
-- First argument is the usage message for when no arguments are given.
command ::  UsageMessage
        ->  String
        ->  ([String] -> Bot ())
        ->  Bot Component
command usage trigger action = mkComponentT $ commandT usage trigger actionT
    where
        actionT :: [String] -> IdentityT Bot ()
        actionT = lift . action

-- | Creates a `BotComponent` that runs its action everytime it's evaluated.
emptyCommand    :: Bot () -> Bot Component
emptyCommand = simpleCommand ""

-- | Similar to `simpleCommand` but allows the action to be wrapped inside of a
-- monad transformer.
simpleCommandT  ::  (BotMonad b)
                =>  String
                ->  b ()
                ->  String -> b ()
simpleCommandT trigger action = commandT NoUsageMessage trigger (const action)

-- | The most general command constructor possible, the result of the action
-- method used here lives inside of a monad transformer.
commandT    ::  (BotMonad b)
            -- | The usage message
            =>  UsageMessage
            -- | The `String` that will trigger this command
            -> String
            -- | The action that should be run when trigger is seen
            ->  ([String] -> b ())
            -- | The resulting
            ->  String -> b ()
commandT usage trigger action = onPrivMsgT (commandAction . words)
    where
        commandAction (first:[])    |   first == trigger    =   getMessage usage
                                    |   otherwise           =   return ()
        commandAction (first:args)  |   first == trigger    =   action args
                                    |   otherwise           =   return ()
        commandAction _             |   trigger == ""       =   action []
                                    |   otherwise           =   return ()

        -- | Unwraps and reports usage messages to the current channel, if
        -- given a message and command with zero arguments. If given no
        -- message, assumes the action takes no arguments and executes it.
        getMessage (UsageMessage msgs)  = liftBot $ mapM_ ircReply msgs
        getMessage NoUsageMessage       = action []
