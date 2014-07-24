{-# LANGUAGE RankNTypes #-}
module Bot.Component.Impl.Reputation (
    reputation
)   where

import Bot.Component
import Bot.Component.Combinator
import Bot.Component.Command
import Bot.Component.Impl.NickCluster
import Bot.Component.Impl.History
import Bot.Component.Stateful
import Bot.IO

import Control.Applicative
import Control.Monad.State
import Data.List
import Data.Maybe
import GHC.Exts
import qualified Data.Map as M
import qualified Data.Sequence as S

-- | Reputation is stored in a mapping from nicks to integers. The nicks are
-- cluster agnostic. Which means that when retrieving the reputation for a given
-- nick, that all of the reputation points for all nicks in that nick's cluster
-- must be summed to get the total reputation for that user.
type Reputation a = BotMonad b => StateT (M.Map Nick Int) b a

help :: HelpMessage
help = HelpMessage {
        canonicalName   = "reputation"
    ,   helpAliases     = ["reputation", "!rep", "rep", "+1", "-1"]
    ,   helpString      = [
            "usage: +1 [nick]"
        ,   "       -1 [nick]"
        ,   "       !rep [nick]"
        ,   ""
        ,   "   The reputation component keeps track of the 'reputation' of"
        ,   "users in the channel. Reputation here being an arbitrary integer"
        ,   "value. Users can grant each other reputation via the `+1` command."
        ,   "If no user is supplied, the last nick that spoke something other"
        ,   "than a `+1` command will gain 1 reputation point. If a nick is"
        ,   "supplied, then that nick will be granted the point."
        ,   ""
        ,   "   The !rep command is used to query reputation scores. If no nick"
        ,   "Is given, then the reputation score of all known nicks will be"
        ,   "displayed. Otherwise, if a nick is given, then only the reputation"
        ,   "score for that user is displayed."
    ]
}

-- | The reputation component. Keeps a mapping of nicks to arbitrary integers.
reputation :: ClusterNickHandle -> HistoryHandle -> Bot Component
reputation nickClusterHandle historyHandle =
        persistent "reputation.txt" (plus +++ minus +++ query) initialState
            `withHelpMessage` help
    where
        initialState :: Bot (M.Map Nick Int)
        initialState = return M.empty

        -- | The +1 command.
        plus :: String -> Reputation ()
        plus = commandT "+1" $ grantReputationAction (+ 1)

        -- | The -1 command.
        minus :: String -> Reputation ()
        minus = commandT "-1" $ grantReputationAction (\x -> x - 1)

        -- | The action that is performed when the string "+1" is said in
        -- a channel.
        grantReputationAction :: (Int -> Int) -> [Nick] -> Reputation  ()
        grantReputationAction delta (nick:_) = grantReputation delta nick
        grantReputationAction delta [] = do
            lastNick <- listToMaybe . map fst <$> validHistory
            fromMaybe (return ()) (Just (grantReputation delta) <*> lastNick)

        -- | Grant a nick one point of reputation. This will either increment
        -- the existing value in the map, or insert 1 if there is no value
        -- present.
        grantReputation :: (Int -> Int) -> Nick -> Reputation ()
        grantReputation delta nick =
            modify $ flip M.alter nick $ return . delta . fromMaybe 0

        -- | Returns a list of messages that are not "+1"'s.
        validHistory :: Reputation [(Nick, Message)]
        validHistory =   filter (not . isPrefixOf "+1" . snd) . lazilyReverseSeq
                     <$> getHistory historyHandle

        -- | Lazily reverse a sequence as a list.
        lazilyReverseSeq :: S.Seq a -> [a]
        lazilyReverseSeq sequence = lazilyReverseSeq' (S.length sequence - 1)
            where
                lazilyReverseSeq' index
                    | index == 0    = [current]
                    | index > 0     = current : lazilyReverseSeq' (index - 1)
                    | otherwise     = []
                    where
                        current = sequence `S.index` index

        -- | The !rep command.
        query :: String -> Reputation ()
        query = commandT "!rep" queryAction

        -- | The action that is performed when the string "!rep" is said in
        -- a channel.
        queryAction :: [Nick] -> Reputation ()
        queryAction (nick:_) = totalForNick nick >>= showReputation nick
        queryAction [] = do
            aliases <- liftBot $ allNickAliases nickClusterHandle
            scores  <- mapM sumForGroup aliases
            let combined    = zip (map head aliases) scores
            let filtered    = filter ((/= 0) . snd) combined
            let sorted      = reverse $ sortWith snd filtered
            mapM_ (uncurry showReputation) sorted

        totalForNick :: Nick -> Reputation Int
        totalForNick nick = do
            aliases <- liftBot $ aliasesForNick nickClusterHandle nick
            sumForGroup aliases

        -- | Return the total reputation score for a list of nicks.
        sumForGroup :: [Nick] -> Reputation Int
        sumForGroup nicks = do
            reputation <- get
            return $ sum $ mapMaybe (`M.lookup` reputation) nicks

        -- | Display the given nick and reputation score in the IRC channel.
        showReputation :: Nick -> Int -> Reputation ()
        showReputation nick reputation =
            liftBot $ ircReply $ nick ++ " has " ++ show reputation ++ " rep"
