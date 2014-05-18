{-# LANGUAGE FlexibleInstances #-}
module Bot.Component.Impl.History (
    HistoryHandle
,   newHistoryHandle
,   historyService
,   getHistory
)   where

import Bot.Component
import Bot.IO

import Control.Applicative
import Control.Concurrent
import Control.Monad.Error
import Control.Monad.State
import Prelude hiding (readFile)
import System.FilePath
import System.IO hiding (readFile)
import System.IO.Strict
import qualified Data.Sequence as S

type Nick     = String
type Message  = String

-- | The internal state of the history service. The datatype is opaque and
-- should only be accessed through the exposed API.
type HistoryInfo = S.Seq (Nick, Message)

-- | Opaque type for the HistoryInfo that is exposed externally.
type HistoryHandle = MVar HistoryInfo

-- | Creates a new HistoryHandle for use with the historyService component.
-- A reference to this required in order to make API calls.
newHistoryHandle :: IO HistoryHandle
newHistoryHandle = liftIO newEmptyMVar

-- | The `BotComponent` portion of the history service. This service must be
-- included in the Bot otherwise all API calls will hang.
historyService :: HistoryHandle -> Bot Component
historyService handle =  loadLog >> mkComponent action
    where
        action = onPrivMsg $ \message -> do
            nick    <- gets currentNick
            logFile <- logPath
            let logLine = nick ++ '\t' : message
            liftIO $ do
                modifyMVar_ handle  (return . (S.|> (nick, message)))
                withFile logFile AppendMode (`hPutStrLn` logLine)

        logPath = do
            directory <- gets dataDirectory
            return (directory </> "log.txt")

        loadLog = do
            logFile <- logPath
            liftIO $ do
                log     <- (map splitLine . lines <$> readFile logFile)
                              `catchError` const (return [])
                putMVar handle (S.fromList log)

        splitLine =   (,)
                  <$> takeWhile (/= '\t')
                  <*> drop 1 . dropWhile (/= '\t')

getHistory  :: (BotMonad m, MonadIO m)
            => HistoryHandle -> m (S.Seq (Nick, Message))
getHistory = liftBot . liftIO . readMVar
