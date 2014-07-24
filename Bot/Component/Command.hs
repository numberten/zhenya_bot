module Bot.Component.Command (
    simpleCommand
,   simpleCommandT
,   emptyCommand
,   command
,   commandT
,   helpForCommand
)   where

import Bot.Component
import Bot.IO

import Control.Monad.Trans
import Control.Monad.Trans.Identity

-- | A convenience function that wraps `command` for commands that don't need to
-- take arguments.
simpleCommand   ::  String
                ->  Bot ()
                ->  Bot Component
simpleCommand trigger action = command trigger (const action)

-- | Create a command `BotComponent` for a command that requires arguments.
command ::  String
        ->  ([String] -> Bot ())
        ->  Bot Component
command trigger action = mkComponentT $ commandT trigger actionT
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
simpleCommandT trigger action = commandT trigger (const action)

-- | The most general command constructor possible, the result of the action
-- method used here lives inside of a monad transformer.
commandT    ::  (BotMonad b)
            -- | The `String` that will trigger this command
            => String
            -- | The action that should be run when trigger is seen
            ->  ([String] -> b ())
            -- | The resulting
            ->  String -> b ()
commandT trigger action = onPrivMsgT (commandAction . words)
    where
        commandAction (first:args)  |   first == trigger    =   action args
                                    |   otherwise           =   return ()
        commandAction _             |   trigger == ""       =   action []
                                    |   otherwise           =   return ()

-- | Creates a help message with a help string for the case where the canonical
-- name is the same as the
helpForCommand :: String -> [String] -> HelpMessage
helpForCommand name helpString = HelpMessage {
    canonicalName   = name
,   helpAliases     = [name, '!':name]
,   helpString      = helpString
}
