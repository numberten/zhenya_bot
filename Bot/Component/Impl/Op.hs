module Bot.Component.Impl.Op (
    grantOps
)   where

import Bot.Component
import Bot.Component.Combinator
import Bot.Component.Command
import Bot.IO

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans.Identity

-- | Grant ops via the !ascend or the !ding command.
grantOps :: Bot Component
grantOps = mkComponentT $ ding +++ ascend
    where
        ding :: String -> IdentityT Bot ()
        ding = simpleCommandT "!ding" dingAction

        dingAction  =   lift
                    $   (++)
                    <$> gets currentChannel
                    <*> ((" +o "++) <$> gets currentNick)
                    >>= ircWrite "MODE"

        ascend :: String -> IdentityT Bot ()
        ascend = simpleCommandT "!ascend" (dingAction >> ascendAction)

        ascendAction = lift $ do
            nick        <-  gets currentNick
            ircReply    $   "Welcome to the ranks of the Ascended, Brother "
                        ++  nick
                        ++  "."

