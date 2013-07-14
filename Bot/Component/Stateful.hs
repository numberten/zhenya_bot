{-# LANGUAGE RecordWildCards #-}
module Bot.Component.Stateful (
    StatefulComponent
,   stateful
,   persistent
)   where

import Bot.Component

import Control.Monad.State
import Control.Monad.Error
import System.FilePath.Posix
import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)

-- TODO: Should this be generalized to be a MonadTrans?
-- It would theoretically be much more flexible, but at the moment I can't think
-- of a use case that would require another monad on top of this...

-- A `BotComponent` that maintains some state.
data StatefulComponent s = StatefulComponent {
        state   :: s 
    ,   action  :: String -> StateT s Bot ()
}

instance Botable (StatefulComponent s) where
    process message StatefulComponent{..} = do
        state <- execStateT (action message) state
        return StatefulComponent {..}

stateful :: (String -> StateT s Bot ()) -> Bot s -> Bot BotComponent
stateful action initialState = do
    state <- initialState
    mkComponent StatefulComponent { .. }

-- A `stateful` `BotComponent` that saves its state in a text file between
-- sessions.
persistent  ::  (Show s, Read s, Eq s)
            =>  FilePath 
            ->  (String -> StateT s Bot ()) 
            ->  Bot s 
            ->  Bot BotComponent
persistent saveFile action initialState = stateful action' initialState'
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
            fileName    <-  lift fullSavePath
            liftIO $ withFile fileName WriteMode (`hPrint` state)

        -- Attempt to read the state from disk. If unsuccessful use the supplied
        -- initialState.
        initialState' = do 
            fileName    <-  fullSavePath
            liftIO (withFile fileName ReadMode loadFile) `catchError` const initialState

        loadFile handle = do
            contents <- hGetContents handle
            return $ read contents

