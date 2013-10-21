{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Bot.Component.Impl.NickCluster (
    ClusterNickHandle
,   newClusterNickHandle
,   clusterNickService
,   aliasesForNick
,   allNickAliases
)   where

import              Bot.Component
import              Bot.Component.Combinator
import              Bot.Component.Command
import              Bot.Component.Stateful
import              Bot.IO

import              Control.Concurrent
import              Control.Exception
import              Control.Monad.State
import              Control.Monad.Trans.Identity
import              Data.Clustering.Hierarchical
import              Data.Char
import              Data.List
import              Data.List.LCS
import qualified    Data.Set as S

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
clusterNickService handle threshold =
        lift (liftIO $ forkIO startClustering)
    >>  persistent nickFile action initial
    where
        nickFile = "nick-cluster.txt"
        delay = 1000000 -- 5 seconds

        -- Create the very first ClusterNickState
        initial =   return S.empty

        -- Updates the ClusterNickInfo to include the latest greatest nick data
        -- at startup.
--        startup = do
--            seenNicks   <-  get
--            info        <-  liftIO $ takeMVar handle
--            liftIO $ putMVar handle info { seenNicks }
--            liftIO $ clusterTimer `catch` handler

        -- The action that is passed to persistent
        action  ::  String -> StateT (S.Set String) (IdentityT Bot) ()
        action  =   nickWatcher
                +++ commandT "!alias" aliasCommand

        -- Add every nick to the cache and to the handle's set of nicks
        nickWatcher _ = do
            BotState{..}                <-  liftBot $ get
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
            mapM_ (liftBot . ircReply) $ S.elems unmatched
            where
                matchedFilter nickSet = not . S.null . S.intersection nickSet
                unmatchedFilter clusters =
                    (`S.notMember` (S.unions $ S.elems clusters))

        -- Pretty prints a Set String of nicks to irc
        replyClusters = liftBot . ircReply . intercalate ", " . S.elems

        -- Create a time to update the ClusterNickInfo and kick off the
        -- clustering call the first time when the component is created
        startClustering = do
            clusterTimer `catch` handler
            threadDelay delay
            startClustering

        handler :: SomeException -> IO ()
        --handler = void . return
        handler = putStrLn . ("ERROR: NickCluster Error: "++) . show

        -- Computes a clustering of all the nicks currently present in the
        -- in the handle. It reads the nicks in a non-blocking manner so that
        -- updates may occur during clustering. Once the clusters have been
        -- computed it updates the handle with the current clustering of nicks.
        clusterTimer = do
            ClusterNickInfo{..} <-  readMVar handle
            let nickList        =   S.elems seenNicks
            let !clusters       =   S.fromList
                                $   map (S.fromList . elements)
                                $   dendrogram SingleLinkage nickList distance
                                    `cutAt` threshold
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
-- supplied nick.
aliasesForNick :: ClusterNickHandle -> String -> Bot [String]
aliasesForNick handle nick = do
    ClusterNickInfo{..} <-  liftIO $ readMVar handle
    return  $ head . (++[[nick]])
            $ map S.elems
            $ S.elems
            $ S.filter (S.member nick) clusters

-- | Returns all of the nick clusters as a list of lists of nicks.
allNickAliases :: ClusterNickHandle -> Bot [[String]]
allNickAliases handle = do
    ClusterNickInfo{..} <-  liftIO $ readMVar handle
    return  $ map S.elems
            $ S.elems
            $ clusters

