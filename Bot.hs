{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Bot (
    Botable (..)
,   BotComponent (..)
,   BotConfig (..)
,   BotState (..)
,   Bot
,   defaultBotConfig
,   withComponents
,   runBot
)   where

import Bot.Component
import Bot.IO

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Data.Maybe
import Network
import System.Exit
import System.IO
import Text.Printf

-- | The configuration used to initialize an instance of a chat bot.
data BotConfig = BotConfig {
        cfgServer      :: String
    ,   cfgPort        :: Int
    ,   cfgNick        :: String
    ,   cfgChannel     :: [String]
    ,   cfgComponents  :: [Bot BotComponent]
}

-- | A default `BotConfig` containing `BotComponent`s that are likely wanted by
-- every bot. Such as pingResponse, 
defaultBotConfig = BotConfig {
        cfgServer       = "localhost"
    ,   cfgPort         = 6667
    ,   cfgNick         = "IRCBot"
    ,   cfgChannel      = []
    ,   cfgComponents   = []
}

-- | There are some components in the `defaultBotConfig` that probably should not
-- be taken out. This function exists to make the process of adding new
-- components easier:
--
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
                ,   exitCode        = Nothing
                ,   currentChannel  = ""
                -- We will update the components in the init method so that they
                -- can be evaluated within the Bot monad
                ,   components      = []
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
            mapM_ (ircWrite "JOIN") cfgChannel
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
            liftM ((return <$>) . void . exitCode) get >>= fromMaybe loop
        
        -- Read a message from IRC and process it with each of the registered
        -- components
        runComponents :: Bot ()
        runComponents = do
            message     <-  ircRead
            -- TODO: add a second catchError wrapper here so that one crappy
            -- component doesn't bring down the entire bot
            components  <-  gets components 
                        >>= mapM (process message)
            modify $ \s -> s { components }

-- | Read from IRC. 
-- This method is not exposed because there should be no reason that a
-- `BotComponent` manually reads from IRC as it's process function is passed
-- every message that is received by the bot.
ircRead :: Bot String
ircRead = do
	handle  <-  gets socket
	message <- init <$> liftIO (hGetLine handle)
	-- force the update of the currentChannel in the BotState
	(\_ -> return ()) `onPrivMsg` message
	liftIO $ printf "< %s\n" message
	return message

