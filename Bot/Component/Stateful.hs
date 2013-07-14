{-# LANGUAGE RecordWildCards #-}
module Bot.Component.Stateful (
    StatefulComponent
,   stateful
)   where

import Bot.Component

import Control.Monad.State

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
stateful action botState = do
    state <- botState
    mkComponent StatefulComponent { .. }

