{-# LANGUAGE FlexibleInstances #-}
module Bot.Component.Function (
)   where

import Bot.Component

import Control.Monad.Trans.Identity

instance Botable (String -> Bot ()) where
    process message method = method message >> return method

instance Botable (String -> IdentityT Bot ()) where
    process message method = runIdentityT (method message) >> return method

