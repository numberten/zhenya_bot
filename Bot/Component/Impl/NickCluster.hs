{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Bot.Component.Impl.NickCluster (
    ClusterNickHandle
,   newClusterNickHandle
,   clusterNickService
,   aliasesForNick
,   clusterForNick
,   allNickAliases
)   where

import Bot.Component
import Bot.Component.Combinator
import Bot.Component.Command
import Bot.Component.Stateful
import Bot.Component.Timer
import Bot.IO

import Control.Applicative
import Control.Concurrent
import Control.Monad.State
import Control.Monad.Trans.Identity
import Data.Clustering.Hierarchical
import Data.Char
import Data.List
import Data.List.LCS
import Data.Maybe
import qualified Data.Set as S

-- | The internal state of the nick clustering service. The datatype is opaque
-- and should only be accessed through the exposed API.
data ClusterNickInfo = ClusterNickInfo {
    seenNicks   :: S.Set String -- ^ All of the nick's we are aware of
,   clusters    :: S.Set (S.Set String) -- ^ A list of all clusters
}

-- | Opaque type for the ClusterNickInfo that is exposed externally.
type ClusterNickHandle = MVar ClusterNickInfo

-- | Creates a new ClusterNickHandle for use with the clusterNickService
-- component. A reference to this required in order to make API calls.
newClusterNickHandle :: IO ClusterNickHandle
newClusterNickHandle =
    liftIO $ newMVar ClusterNickInfo {seenNicks = S.empty, clusters = S.empty}

-- | The `BotComponent` portion of the nick clustering service. This service
-- must be included in the Bot otherwise all API calls will hang.
clusterNickService :: ClusterNickHandle -> Double -> Bot Component
clusterNickService handle threshold =   ioTimer "NickCluster" delay clusterTimer
                                    >>  persistent nickFile action initial
    where
        nickFile = "nick-cluster.txt"
        delay = 1000000 -- 1 second

        -- Create the very first ClusterNickState
        initial =   return S.empty

        -- The action that is passed to persistent
        action  ::  String -> StateT (S.Set String) (IdentityT Bot) ()
        action  =   nickWatcher
                +++ commandT NoUsageMessage "!alias" aliasCommand
                +++ commandT usage "!forget" forgetCommand

        -- The usage message for !forget, in case no arguments are passed.
        usage = UsageMessage ["usage: !forget nick"]

        -- Add every nick to the cache and to the handle's set of nicks
        nickWatcher _ = do
            BotState{..}                <-  liftBot get
            info@ClusterNickInfo{..}    <-  liftBot $ liftIO $ takeMVar handle
            seenNicks                   <-  liftM (addNick currentNick) get
            liftBot $ liftIO $ putMVar handle info { seenNicks }
            put seenNicks
            where
                addNick currentNick = S.filter (/= "") . S.insert currentNick


        -- Queries the alias clusters, if there is no arguments, it will list
        -- every cluster. Otherwise, it will list aliases for each name given.
        aliasCommand []     = do
            ClusterNickInfo{..} <-  liftBot $ liftIO $ readMVar handle
            mapM_ replyClusters $ S.elems clusters

        aliasCommand nicks  = do
            ClusterNickInfo{..} <-  liftBot $ liftIO $ readMVar handle
            let nickSet         =   S.fromList nicks
            let matches         =   S.filter (matchedFilter nickSet) clusters
            let unmatched       =   S.filter (unmatchedFilter clusters) nickSet
            mapM_ replyClusters $ S.elems matches
            unless (S.null unmatched) $ liftBot $ ircReply $
                "No cluster for: " ++ (intercalate ", " $ S.elems unmatched)
            where
                matchedFilter nickSet = not . S.null . S.intersection nickSet
                unmatchedFilter clusters =
                    (`S.notMember` (S.unions $ S.elems clusters))

        -- Removes a nick from the set of known nicks. It is sometimes necessary
        -- to manually curate the set of nicks to remove nicks that cause
        -- clusters to clash.
        forgetCommand nicks =   modify (flip (foldr S.delete) nicks)
                            >>  liftBot (ircReply "whoops")

        -- Pretty prints a Set String of nicks to irc
        replyClusters = liftBot . ircReply . intercalate ", " . S.elems

        -- Computes a clustering of all the nicks currently present in the
        -- in the handle. It reads the nicks in a non-blocking manner so that
        -- updates may occur during clustering. Once the clusters have been
        -- computed it updates the handle with the current clustering of nicks.
        clusterTimer = do
            ClusterNickInfo{..} <-  readMVar handle
            let nickList        =   S.elems seenNicks
            let clusters        =   S.fromList
                                $   map (S.fromList . elements)
                                $   dendrogram CompleteLinkage nickList distance
                                    `cutAt` threshold
            unless (null nickList) $
                modifyMVar_ handle (\info -> return info {clusters})

        -- The distance function used for clustering. The distance between two
        -- nicks a and b is defined to be 1 - lcs(a,b)/(min(|a|,|b|). Or in
        -- other words 1 minus the ratio of the shorter nick appearing in the
        -- longer.
        distance a b = 1 - (overlap / min lengthA lengthB)
            where
                a'      = map toLower a
                b'      = map toLower b
                overlap = fromIntegral $ length $ lcs a' b'
                lengthA = fromIntegral $ length a'
                lengthB = fromIntegral $ length b'

-- | Returns a list of nicks that have been determined to be aliases for the
-- supplied nick. Similar to `clusterForNick` with the exception that if the
-- nick does not have a corresponding cluster the singleton list of the given
-- nick is returned.
aliasesForNick :: ClusterNickHandle -> String -> Bot [String]
aliasesForNick handle nick = fromMaybe [nick] <$> clusterForNick handle nick

-- | Returns the cluster that the nick belongs to. If the nick does not belong
-- to a cluster then Nothing is returned.
clusterForNick :: ClusterNickHandle -> String -> Bot (Maybe [String])
clusterForNick handle nick = do
    ClusterNickInfo{..} <-  liftIO $ readMVar handle
    return  $ listToMaybe
            $ take 1
            $ map S.elems
            $ S.elems
            $ S.filter (S.member nick) clusters

-- | Returns all of the nick clusters as a list of lists of nicks.
allNickAliases :: ClusterNickHandle -> Bot [[String]]
allNickAliases handle = do
    ClusterNickInfo{..} <-  liftIO $ readMVar handle
    return  $ map S.elems
            $ S.elems clusters
