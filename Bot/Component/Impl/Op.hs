module Bot.Component.Impl.Op (
    grantOps
)   where

import Bot.Component
import Bot.Component.Combinator
import Bot.Component.Command
import Bot.Component.Impl.Stalker
import Bot.IO

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans.Identity

-- | Grant ops via the !ascend or the !ding command.
grantOps :: StalkerHandle -> Bot Component
grantOps stalker = mkComponentT $ ding +++ ascend +++ oprah
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

        oprah :: String -> IdentityT Bot ()
        oprah = simpleCommandT "!oprah" oprahAction

        oprahAction =   lift $ do
            channel <- gets currentChannel
            nicks   <- getNicksOnChannel stalker channel
            botNick <- gets botNick
            let ops = map (giveOp channel)
                    . filter (/= botNick)
                    . map snd
                    . filter ((/= Op) . fst)
                    $ nicks
            sequence_ ops
            ircReply "EVERYONE GETS AN OP!!!!!!"
            where
                giveOp channel nick = do
                    ircReply "You get an op!"
                    ircWrite "MODE" $ channel ++ " +o " ++ nick
