{-# LANGUAGE RecordWildCards #-}
module Bot.Component.Impl.Seen (
    seen
)   where

import Bot.Component
import Bot.Component.Command
import Bot.Component.Conditional
import Bot.Component.Combinator
import Bot.Component.Impl.NickCluster
import Bot.Component.Stateful
import Bot.IO
import Bot.Time

import Control.Arrow hiding ((+++))
import Control.Monad.State
import Control.Monad.Trans.Identity
import Data.List
import Data.Maybe
import System.Time
import qualified Data.Map as M

-- | The standard ClockTime definition does not define a Read Instance?? So to
-- we redefine both the Show and Read in a newtype.
newtype SeenTime = SeenTime ClockTime
    deriving (Eq)

instance Show SeenTime where
    show (SeenTime (TOD a b)) = show (a,b)

instance Read SeenTime where
    readsPrec precedence string = do
        ((a,b), leftOver)   <-  readsPrec precedence string
        return (SeenTime $ TOD a b, leftOver)

type TimeMap = M.Map String SeenTime

-- | The seen component keeps a mapping of nicks to times that they last spoke.
-- And can be queried through the !seen command.
seen :: ClusterNickHandle -> Bot Component
seen cnHandle = persistent "seen.txt" action initialState
    where
        action          = seenLogger +++ seenCommand cnHandle
        initialState    = return M.empty

-- | Runs for every message updates the time last seen for the current nick.
seenLogger :: String -> StateT TimeMap (IdentityT Bot) ()
seenLogger = conditionalT (const True) seenLoggerAction

seenLoggerAction :: StateT TimeMap (IdentityT Bot) ()
seenLoggerAction = do
    BotState{..}    <-  lift get
    now             <-  liftIO getClockTime
    modify (M.insert currentNick $ SeenTime now)

-- | Responds to !seen commands.
seenCommand :: ClusterNickHandle -> String -> StateT TimeMap (IdentityT Bot) ()
seenCommand cnHandle = commandT usage "!seen" $ seenCommandAction cnHandle

-- The usage message, in case no arguments are passed.
usage = UsageMessage ["usage: !seen nick"]
seenCommandAction   ::  ClusterNickHandle
                    ->  [String]
                    ->  StateT TimeMap (IdentityT Bot) ()
seenCommandAction cnHandle (nick:_) = do
    timeMap         <-  get
    aliases         <-  liftBot $ aliasesForNick cnHandle nick
    let maybeFind   =   mfuse . (return &&& (`M.lookup` timeMap))
    let seenTimes   =   mapMaybe maybeFind aliases
    case seenTimes of
        []          -> liftBot $ ircReply "I have not seen them speak."
        seenTimes   -> do
            let (nick, SeenTime lastSeen)
                        =   maximumBy compareTimes seenTimes
            now         <-  liftIO getClockTime
            let diff    =   pretty $ diffClockTimes now lastSeen
            liftBot $ ircReply $ concat
                ["I last saw them speak ", diff, " ago as ", nick]
    where
        compareTimes (_, SeenTime a) (_, SeenTime b) = compare a b
        mfuse (ma,mb) = do
            a <- ma
            b <- mb
            return (a,b)

seenCommandAction _       _         = return ()

