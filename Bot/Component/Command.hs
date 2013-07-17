module Bot.Component.Command (
    simpleCommand
,   simpleCommandT
,   emptyCommand
,   command
,   commandT
)   where

import Bot.Component
import Bot.Component.Function()
import Bot.IO

import Control.Monad.Trans
import Control.Monad.Trans.Identity

-- | A convenience function that wraps `command` for commands that don't need to
-- take arguments.
simpleCommand :: String -> Bot () -> Bot BotComponent
simpleCommand trigger action = command trigger (\_ -> action)

-- | Create a command `BotComponent` for a command that requires arguments.
command :: String -> ([String] -> Bot ()) -> Bot BotComponent
command trigger action = mkComponent $ commandT trigger actionT
    where
        actionT :: [String] -> IdentityT Bot ()
        actionT = lift . action

-- | Creates a `BotComponent` that runs its action everytime it's evaluated.
emptyCommand :: Bot () -> Bot BotComponent
emptyCommand = simpleCommand ""

-- | Similar to `simpleCommand` but allows the action to be wrapped inside of a
-- monad transformer.
simpleCommandT  ::  (MonadTrans t, Monad (t Bot)) 
                =>  String 
                ->  t Bot () 
                ->  String -> t Bot ()
simpleCommandT trigger action = commandT trigger (\_ -> action)

-- | The most general command constructor possible, the result of the action
-- method used here lives inside of a monad transformer.
commandT    ::  (MonadTrans t, Monad (t Bot))
            -- | The `String` that will trigger this command
            =>  String 
            -- | The action that should be run when trigger is seen
            ->  ([String] -> t Bot ()) 
            -- | The resulting 
            -> String -> t Bot ()
commandT "" action      = action . words
commandT trigger action = onPrivMsgT (commandAction . words)
    where
        commandAction (first:args)  |   first == trigger    =   action args
                                    |   otherwise           =   return ()
        commandAction _                                     =   return ()

