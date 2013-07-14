module Bot.Component.Conditional (
    conditional
,   conditionalT
)   where

import Bot.Component
import Bot.Component.Function()
import Bot.IO

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Identity

-- | Similar to conditionalT but less generic and easier to use.
conditional  :: (String -> Bool) -> Bot () -> Bot BotComponent
conditional predicate action = 
    mkComponent $ conditionalT predicate actionT
    where
        actionT :: IdentityT Bot ()
        actionT = lift action

-- | A generic conditional component that will execute an action when a given
-- predicate over a PRIVMSG message is satisfied.
conditionalT ::  (MonadTrans t, Monad (t Bot))
            -- | The predicate that determines if the specified action is
            -- allowed to run.
            =>  (String -> Bool)
            -- | The action to be executed in the event of a match
            ->  t Bot () 
            -- | Resulting Botable method
            ->  String -> t Bot ()
conditionalT predicate action  =  onPrivMsgT $ flip when action . predicate

