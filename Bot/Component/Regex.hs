module Bot.Component.Regex (
    regex
,   regexT
)   where

import Bot.Component
import Bot.Component.Function()
import Bot.IO

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Identity
import Text.Regex.TDFA

type Pattern = String

-- | Given a regular expression and an action, create a `BotComponent` that will
-- execute the action for each encountered substring that matches the pattern.
regex :: Pattern -> (String -> Bot ()) -> Bot BotComponent
regex pattern action = mkComponent $ regexT pattern actionT
    where
        actionT :: String -> IdentityT Bot ()
        actionT = lift . action

-- | A general regex matching constructor. For each substring in the message
-- that matches will be passed to the action method.
regexT ::  (MonadTrans t, Monad (t Bot))
       -- | The predicate that determines if the specified action is
       -- allowed to run.
       =>  Pattern
       -- | The action to be executed for every substring that matches the regex
       ->  (String -> t Bot ())
       -- | Resulting Botable method
       ->  String -> t Bot ()
regexT pattern action   =   onPrivMsgT
                        $   mapM_ action
                        .   concat . (=~ pattern)

