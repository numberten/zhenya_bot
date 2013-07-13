module Bot.Component.Command (
    Command
,   simpleCommand
,   command
)   where

import Bot.Component
import Bot.IO

-- | A `Command` is an action that is triggered when the first word of a message
-- is equal to a given string.
newtype Command = Command (String, [String] -> Bot ())

instance Botable Command where
    process message component@(Command (trigger,action)) 
        =   message `onPrivMsg` (doAction . words)
        >>  return component
        where
            doAction (first:args)   | first == trigger  = action args
                                    | otherwise         = return ()
            doAction _                                  = return ()

-- | A convenience function that wraps `command` for commands that don't need to
-- take arguments.
simpleCommand :: String -> Bot () -> Bot BotComponent
simpleCommand trigger action = command trigger (\_ -> action)

-- | Create a command `BotComponent` for a command that requires arguments.
command :: String -> ([String] -> Bot ()) -> Bot BotComponent
command command action  =   return 
                        $   MkBotComponent
                        $   Command (command, action)

