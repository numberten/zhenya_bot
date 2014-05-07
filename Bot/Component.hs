{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Bot.Component (
    Bot
,   BotState (..)
,   Component (..)
,   ComponentPart
,   BotMonad (..)
,   liftIO
,   mkComponent
,   mkComponentT
)   where

import Control.Applicative
import Control.Concurrent.STM.TChan
import Control.Monad.State
import Control.Monad.Trans.Identity
import System.Exit
import System.IO

-- | The internal state of the IRC Bot
data BotState = BotState {
        socket          :: Handle
    -- | If this value is not Nothing, then the bot will exit with the given
    -- `ExitCode`
    ,   exitCode        :: Maybe ExitCode
    ,   dataDirectory   :: FilePath
    ,   components      :: [Component]
    ,   currentChannel  :: String
    ,   currentNick     :: String
    ,   botNick         :: String
    ,   botHost         :: String
    ,   messageQueue    :: TChan String
}

-- | A type synonym for the Bot monad.
type Bot = StateT BotState IO

class (Monad b) => BotMonad b where
    -- | Anything required to turn an instance of b a into Bot a
    type BotExtractor b :: *

    -- | Similar to liftIO, the recursive definition is suitable for every
    -- instance of BotMonad except for the base case of `IdentityT Bot`.
    liftBot :: Bot a -> b a

    -- | Escape out of the monad tranformer stack down to a plain ol' Bot
    -- monadic value.
    dropBot :: BotExtractor b -> b a -> Bot (a, BotExtractor b)

    dropBot' :: BotExtractor b -> b () -> Bot (BotExtractor b)
    dropBot' a b = snd <$> dropBot a b

instance BotMonad (IdentityT Bot) where
    type BotExtractor (IdentityT Bot) = ()
    liftBot = lift
    dropBot _ = ((,()) <$>) . runIdentityT

-- | A `Component` represents an isolated IRC bot feature.
--
-- This type combines a BotDrop value for lowering a MonadBot value to a Bot
-- value and a method that takes a raw IRC message returns a new BotDrop value.
--
-- The existential is necessary for making the types work out when storing
-- heterogeneous MonadBot types in the same collection.
type ComponentPart b = (BotExtractor b, String -> b ())

data Component = forall b . BotMonad b => MkComponent (ComponentPart b)

-- | Takes a `` type and creates a `Bot BotComponent` that can be used
-- with `withComponents`.
mkComponent ::  (String -> Bot ())
            ->  Bot Component
mkComponent = mkComponentT . (IdentityT .)

-- | Takes a `` type and creates a `Bot BotComponent` that can be used
-- with `withComponents`.
mkComponentT    ::  (BotMonad b, BotExtractor b ~ ())
                =>  (String -> b ())
                ->  Bot Component
mkComponentT action = return $ MkComponent ((), action)
