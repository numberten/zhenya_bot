{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
module Bot.Component.Stateful (
    stateful
,   statefulP
,   persistent
,   persistentP
--,   persistent'
)   where

import Bot.Component

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Trans.Identity
import System.FilePath.Posix
import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)

-- | Declare StateT as an instance of MonadBot.
instance BotMonad b => BotMonad (StateT s b) where
    -- | All we need to drop out of the StateT monad is the current state and
    -- what ever inner state the inner monad has.
    type BotExtractor (StateT s b) = (s, BotExtractor b)

    -- | Dropping out of the StateT monad is done by dropping out of the outer
    -- StateT monad and then doing whatever is required to drop out of the inner
    -- monad.
    dropBot (state, innerState) =   (((,)   <$> fst . fst
                                            <*> ((,) <$> snd . fst <*> snd))
                                    <$>) -- ugly tuple juggling
                                <$> dropBot innerState
                                .   (`runStateT` state)

    liftBot = lift . liftBot

statefulP   ::  BotMonad b
            =>  (String -> StateT s b ())
            ->  Bot s
            ->  BotExtractor b -> Bot (ComponentPart (StateT s b))
statefulP action initialState innerState = do
    state <- initialState
    return ((state, innerState), action)

stateful    ::  (String -> StateT s (IdentityT Bot) ())
            ->  Bot s
            ->  Bot Component
stateful action initialState = MkComponent <$> statefulP action initialState ()

-- A `stateful` `BotComponent` that saves its state in a text file between
-- sessions.
persistent  ::  (Show s, Read s, Eq s)
            =>  FilePath
            ->  (String -> StateT s (IdentityT Bot) ())
            ->  Bot s
            ->  Bot Component
persistent saveFile action initialState =
    MkComponent <$> persistentP saveFile action initialState ()

-- A `stateful` `BotComponent` that saves its state in a text file between
-- sessions that also has a startupAction. This is useful is there is some
-- post-processing that needs to happen after the state is loaded from a file.
persistentP ::  (Show s, Read s, Eq s, BotMonad b)
            =>  FilePath
            ->  (String -> StateT s b ())
            ->  Bot s
            ->  BotExtractor b ->  Bot (ComponentPart (StateT s b))
persistentP saveFile action initialState =
    statefulP action' initialState'
    where
        fullSavePath = do
            directory   <-  gets dataDirectory
            return (directory </> saveFile)

        -- Perform the action, but also save the state to disk if the state has
        -- changed.
        action' message = do
            oldState <- get
            action message
            get >>= flip when saveState . (oldState /=)

        saveState = do
            state       <-  get
            fileName    <-  liftBot fullSavePath
            liftBot $ liftIO $ withFile fileName WriteMode (`hPrint` state)

        -- Attempt to read the state from disk. If unsuccessful use the supplied
        -- initialState.
        initialState' = do
            fileName    <-  fullSavePath
            liftIO (withFile fileName ReadMode loadFile)
                `catchError` const initialState

        loadFile handle = do
            contents <- hGetContents handle
            return $ read contents
