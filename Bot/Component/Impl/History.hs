{-# LANGUAGE FlexibleInstances #-}
module Bot.Component.Impl.History (
    HistoryHandle
,   newHistoryHandle
,   historyService
,   getHistory
)   where

import Bot.Component
import Bot.IO

import Control.Concurrent
import Control.Monad.State
import qualified Data.Sequence as S

type Nick     = String
type Message  = String

-- | The internal state of the history service. The datatype is opaque and
-- should only be accessed through the exposed API.
type HistoryInfo = S.Seq (Nick, Message)

-- | Opaque type for the ClusterNickInfo that is exposed externally.
type HistoryHandle = MVar HistoryInfo

-- | Creates a new ClusterNickHandle for use with the clusterNickService
-- component. A reference to this required in order to make API calls.
newHistoryHandle :: IO HistoryHandle
newHistoryHandle = liftIO $ newMVar S.empty

-- | The `BotComponent` portion of the history service. This service must be
-- included in the Bot otherwise all API calls will hang.
historyService :: HistoryHandle -> Bot Component
historyService handle = mkComponent $ onPrivMsg $ \message -> do
    nick <- gets currentNick
    liftIO $ modifyMVar_ handle  (return . (S.|> (nick, message)))

getHistory  :: (BotMonad m, MonadIO m)
            => HistoryHandle -> m (S.Seq (Nick, Message))
getHistory = liftBot . liftIO . readMVar
