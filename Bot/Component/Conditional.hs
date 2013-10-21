module Bot.Component.Conditional (
    conditional
,   conditionalT
)   where

import Bot.Component
import Bot.IO

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Identity

-- | Similar to conditionalT but less generic and easier to use.
conditional  :: (String -> Bool) -> Bot () -> Bot Component
conditional predicate action =
    mkComponentT $ conditionalT predicate actionT
    where
        actionT :: IdentityT Bot ()
        actionT = lift action

-- | A generic conditional component that will execute an action when a given
-- predicate over a PRIVMSG message is satisfied.
conditionalT ::  BotMonad b
            -- | The predicate that determines if the specified action is
            -- allowed to run.
            =>  (String -> Bool)
            -- | The action to be executed in the event of a match
            ->  b ()
            -- | Resulting Botable method
            ->  String -> b ()
conditionalT predicate action  =  onPrivMsgT $ flip when action . predicate

