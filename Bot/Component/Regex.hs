module Bot.Component.Regex (
    regex
,   regexT
)   where

import Bot.Component
import Bot.IO

import Control.Monad.Trans
import Control.Monad.Trans.Identity
import Text.Regex.TDFA

type Pattern = String

-- | Given a regular expression and an action, create a `BotComponent` that will
-- execute the action for each encountered substring that matches the pattern.
regex :: Pattern -> (String -> Bot ()) -> Bot Component
regex pattern action = mkComponentT $ regexT pattern actionT
    where
        actionT :: String -> IdentityT Bot ()
        actionT = lift . action

-- | A general regex matching constructor. For each substring in the message
-- that matches will be passed to the action method.
regexT ::  BotMonad b
       -- | The predicate that determines if the specified action is
       -- allowed to run.
       =>  Pattern
       -- | The action to be executed for every substring that matches the regex
       ->  (String -> b ())
       -- | Resulting Botable method
       ->  String -> b ()
regexT pattern action   =   onPrivMsgT
                        $   mapM_ action
                        .   concat . (=~ pattern)

