{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Bot (
    Botable
,   BotComponent
,   BotConfig (..)
,   defaultBotConfig
,   withComponents
,   runBot
) where

import Control.Applicative
import Data.Map
import Network
import System.Exit
import System.IO
import System.Time

-- | A type class for wrapping bot functionality. 
-- The idea is that each distinct piece of functionality would have it's own
-- value where the type would be of the Botable class. This allows the BotConfig
-- to maintain a heterogeneous list of Botable values each of which containing
-- potentially unique state.
class Botable a where
    -- | Process a raw IRC message. The function is responsible for parsing out
    -- the cruft of the IRC protocol.
    process :: String -> a -> IO a

-- | A type that wraps Botable types.
-- This type is necessary for making the types work out when storing
-- heterogeneous Botable types in the same collection.
data BotComponent = forall a . Botable a => MkBotable a

-- Make 'BotComponent' an instance of Botable such that it recursively attempts
-- to process the line as the inner Botable value.
instance Botable BotComponent where
    process line (MkBotable a)  =   process line a `asTypeOf` return a
                                >>= return . MkBotable

-- | Used to initialize an instance of a chat bot.
data BotConfig = BotConfig {
        cfgServer      :: String
    ,   cfgPort        :: Int
    ,   cfgNick        :: String
    ,   cfgChannel     :: String
    ,   cfgComponents  :: [(String,BotComponent)]
}

defaultBotConfig = BotConfig {
        cfgServer       = "localhost"
    ,   cfgPort         = 6667
    ,   cfgNick         = "IRCBot"
    ,   cfgChannel      = "#robotz"
    ,   cfgComponents   = []
}

-- | The internal state of the IRC Bot
data BotState = BotState { 
        socket      :: Handle
    ,   startTime   :: ClockTime
    -- | If this value is not Nothing, then the bot will exit with the given
    -- `ExitCode`
    ,   exitCode    :: Maybe ExitCode
    ,   components  :: Map String BotComponent 
}

-- | There are some components in the defaultBotConfig that probably should not
-- be taken out. This function exists to make the process of adding new
-- components easier:
-- > defaultBotConfig `withComponents` [
-- >        component1
-- >    ,   component2
-- > ]
withComponents  :: BotConfig -> [(String,BotComponent)] -> BotConfig
withComponents cfg additional = cfg {
        cfgComponents = cfgComponents cfg ++ additional
    }

-- | Launch the IRC bot.
runBot :: BotConfig -> IO ()
runBot config = return ()

