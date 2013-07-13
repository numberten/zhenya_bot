{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
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
    process :: String -> a -> Bot a

-- | A type that wraps Botable types.
-- This type is necessary for making the types work out when storing
-- heterogeneous Botable types in the same collection.
data BotComponent = forall a . Botable a => MkBotable a

-- Make `BotComponent` an instance of Botable such that it recursively attempts
-- to process the line as the inner Botable value.
instance Botable BotComponent where
    process line (MkBotable a)  =   liftM MkBotable 
                                $   process line a `asTypeOf` return a

-- | The configuration used to initialize an instance of a chat bot.
data BotConfig = BotConfig {
        cfgServer      :: String
    ,   cfgPort        :: Int
    ,   cfgNick        :: String
    ,   cfgChannel     :: [String]
    ,   cfgComponents  :: [Bot BotComponent]
}

-- | The internal state of the IRC Bot
data BotState = BotState { 
        socket      :: Handle
    -- | If this value is not Nothing, then the bot will exit with the given
    -- `ExitCode`
    ,   exitCode    :: Maybe ExitCode
    ,   components  :: [BotComponent]
}

-- | A type synonym for the Bot monad.
type Bot a = StateT BotState IO a

-- | A default `BotConfig` containing `BotComponent`s that are likely wanted by
-- every bot. Such as pingResponse, 
defaultBotConfig = BotConfig {
        cfgServer       = "localhost"
    ,   cfgPort         = 6667
    ,   cfgNick         = "IRCBot"
    ,   cfgChannel      = []
    ,   cfgComponents   = []
}

-- | There are some components in the defaultBotConfig that probably should not
-- be taken out. This function exists to make the process of adding new
-- components easier:
-- > defaultBotConfig `withComponents` [
-- >        component1
-- >    ,   component2
-- > ]
withComponents  :: BotConfig -> [Bot BotComponent] -> BotConfig
withComponents cfg additional = cfg {
        cfgComponents = cfgComponents cfg ++ additional
    }

-- | Launch the IRC bot.
runBot :: BotConfig -> IO ()
runBot BotConfig{..}    =   connect
                        >>= execStateT (init cfgComponents >> loop)
                        >>= disconnect
    where
        -- Connect to the server and create the resulting `BotState`
        connect = do
            socket      <-  connectTo cfgServer 
                        $   PortNumber (fromIntegral cfgPort)
            hSetBuffering socket NoBuffering
            return BotState {
                    socket
                ,   exitCode    = Nothing
                -- We will update the components in the init method so that they
                -- can be evaluated within the Bot monad
                ,   components  = []
            }

        -- Close the socket and return with the desired exit code.
        disconnect BotState{..} = do
            hClose socket
            exitWith $ fromMaybe ExitSuccess exitCode

        -- Connect to the desired channels, and set up the nick
        init :: [Bot BotComponent] -> Bot ()
        init cfgComponents = do
            ircWrite "NICK" cfgNick
            ircWrite "USER" $ cfgNick ++ " 0 * :Greatest Guys bot"
            mapM (ircWrite "JOIN") cfgChannel
            components  <-  sequence cfgComponents
            modify $ \s -> s { components }

        -- Grab messages off IRC and pass them to various `BotComponent`s
        loop :: Bot ()
        loop = do
            catchError
                    runComponents
                $   \e  ->  liftIO (print e)
                        >>  modify (\s -> s {exitCode = Just $ ExitFailure 1})
            -- Continue to loop if there is no exit code, other wise stop.
            get >>= return . (return <$>) . void . exitCode >>= fromMaybe loop
        
        -- Read a message from IRC and process it with each of the registered
        -- components
        runComponents :: Bot ()
        runComponents = do
            message     <-  ircRead
            components  <-  gets components 
                        >>= mapM (process message)
            modify $ \s -> s { components }

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

