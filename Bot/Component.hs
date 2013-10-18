{-# LANGUAGE ExistentialQuantification #-}
module Bot.Component (
    Botable (..)
,   BotComponent (..)
,   BotState (..)
,   Bot
,   mkComponent
,   liftIO
)   where

import Control.Monad
import Control.Monad.State
import System.Exit
import System.IO

-- | A type class for wrapping bot functionality. 
-- The idea is that each distinct piece of functionality would have it's own
-- value where the type would be of the Botable class. This allows the BotConfig
-- to maintain a heterogeneous list of Botable values each of which containing
-- potentially unique state.
class Botable a where
    -- | Process a raw IRC message. The function is responsible for parsing out
    -- the cruft of the IRC protocol.
    process :: String -> a -> Bot a

-- | A type that wraps Botable types.
-- This type is necessary for making the types work out when storing
-- heterogeneous Botable types in the same collection.
data BotComponent = forall a . Botable a => MkBotComponent a

-- Make `BotComponent` an instance of Botable such that it recursively attempts
-- to process the line as the inner Botable value.
instance Botable BotComponent where
    process line (MkBotComponent a) =   liftM MkBotComponent 
                                    $   process line a `asTypeOf` return a

-- | The internal state of the IRC Bot
data BotState = BotState { 
        socket          :: Handle
    -- | If this value is not Nothing, then the bot will exit with the given
    -- `ExitCode`
    ,   exitCode        :: Maybe ExitCode
    ,   dataDirectory   :: FilePath
    ,   components      :: [BotComponent]
    ,   currentChannel  :: String
    ,   currentNick     :: String
    ,   botNick         :: String
    ,   botHost         :: String
}

-- | A type synonym for the Bot monad.
type Bot = StateT BotState IO

-- | Takes a `Botable` type and creates a `Bot BotComponent` that can be used
-- with `withComponents`.
mkComponent :: forall b . Botable b => b -> Bot BotComponent
mkComponent botable = return $ MkBotComponent botable

