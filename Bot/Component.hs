{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Bot.Component (
    Bot
,   BotState (..)
,   Component (..)
,   ComponentPart
,   HelpMessage (..)
,   BotMonad (..)
,   liftIO
,   mkComponent
,   mkComponentT
,   withHelpMessage
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

-- | At a high level, `ComponentPart` is a function that takes an IRC line and
-- returns something that can be turned into a BotMonad.
--
-- The existential is necessary for making the types work out when storing
-- heterogeneous MonadBot types in the same collection.
type ComponentPart b = (BotExtractor b, String -> b ())

-- | The help message associated with this component. There can either be no
-- help string, or a canonical name, a list of aliases, and a list of lines that
-- are the help message.
data HelpMessage = HelpMessage {
      canonicalName :: String
    , helpAliases   :: [String]
    , helpString    :: [String]
  }

-- | A `Component` represents an isolated IRC bot feature. It wraps
-- `ComponentPart` in an existential so that heterogeneous collections of
-- `ComponentPart`s can be contained in the same list.
--
-- The existential is necessary for making the types work out when storing
-- heterogeneous MonadBot types in the same collection.
data Component =  forall b . BotMonad b
               => MkComponent (ComponentPart b) (Maybe HelpMessage)

-- | Takes a `` type and creates a `Bot BotComponent` that can be used
-- with `withComponents`.
mkComponent ::  (String -> Bot ())
            ->  Bot Component
mkComponent = mkComponentT . (IdentityT .)

-- | Takes a `ComponentPart` and creates a `Bot BotComponent` that can be used
-- with `withComponents`.
mkComponentT    ::  (BotMonad b, BotExtractor b ~ ())
                =>  (String -> b ())
                ->  Bot Component
mkComponentT action = return $ MkComponent ((), action) Nothing

-- | Annotates a component with a help message.
withHelpMessage :: Bot Component -> HelpMessage -> Bot Component
withHelpMessage component help = annotate <$> component
  where annotate (MkComponent part _) = MkComponent part (Just help)
