module Bot.Component.Command (
    SimpleCommand
,   simpleCommand
)   where

import Bot.Component
import Bot.IO

import Control.Monad.State

-- | A `SimpleCommand` is an action that is triggered on  
newtype SimpleCommand = SimpleCommand (String, Bot ())

instance Botable SimpleCommand where
    process message component@(SimpleCommand (command,action)) 
        =   message `onPrivMsg` doAction 
        >>  return component
        where
            doAction message
                | message == command    = action
                | otherwise             = liftIO $ putStrLn ("NOT COMMAND!: " ++ message)

-- | You should not instantiate a SimpleCommand using the type constructor.
-- Instead you should use this method because it automatically handles
-- converting it to a `Bot BotComponent`
simpleCommand :: String -> Bot () -> Bot BotComponent
simpleCommand command action    =   return 
                                $   MkBotComponent
                                $   SimpleCommand (command,action)

