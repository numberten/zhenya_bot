{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Bot (
    Botable (..)
,   BotComponent (..)
,   BotConfig (..)
,   BotState (..)
,   defaultBotConfig
,   withComponents
,   runBot
,   ircWrite
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Data.Map
import Data.Maybe
import Network
import System.Exit
import System.IO
import System.Time
import Text.Printf

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
    process line (MkBotable a)  =   liftM MkBotable 
                                $   process line a `asTypeOf` return a

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

-- | A type synonym for the Bot monad.
type Bot a = StateT BotState IO a

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
runBot BotConfig{..}    =   connect
                        >>= execStateT (init >> loop)
                        >>= disconnect
    where
        -- Connect to the server and create the resulting `BotState`
        connect = do
            t <- getClockTime
            h <- connectTo cfgServer (PortNumber $ fromIntegral cfgPort)
            hSetBuffering h NoBuffering
            return BotState {
                    socket      = h
                ,   startTime   = t
                ,   exitCode    = Nothing
                ,   components  = fromList cfgComponents
            }

        -- Close the socket and return with the desired exit code.
        disconnect BotState{..} = do
            hClose socket
            exitWith $ fromMaybe ExitSuccess exitCode

        -- Connect to the desired channels, and set up the nick
        init :: Bot ()
        init = do
            ircWrite "NICK" cfgNick
            ircWrite "USER" $ cfgNick ++ " 0 * :Greatest Guys bot"
            ircWrite "JOIN" cfgChannel

        -- Grab messages off IRC and pass them to various `BotComponent`s
        loop :: Bot ()
        loop = do
            catchError
                    runComponents
                $   \e  ->  liftIO (print e)
                        >>  modify (\s -> s {exitCode = Just $ ExitFailure 1})
            -- Continue to loop if there is no exit code, other wise stop.
            get >>= liftM2 fromMaybe loop . (return . void . exitCode)
        
        -- Read a message from IRC and process it with each of the registered
        -- components
        runComponents :: Bot ()
        runComponents = do
            message <- ircRead
            return ()

-- | Write a `String` message to IRC.
ircWrite :: String -> String -> Bot ()
ircWrite command message = do
	handle  <-  gets socket
	liftIO  $   hPrintf handle "%s %s\r\n" command message
	        >>  printf "> %s %s\n" command message

-- | Read from IRC. 
-- This method is not exposed because there should be no reason that a
-- `BotComponent` manually reads from IRC as it's process function is passed
-- every message that is received by the bot.
ircRead :: Bot String
ircRead = do
	handle  <-  gets socket
	message <- init <$> liftIO (hGetLine handle)
	liftIO $ printf "< %s\n" message
	return message

