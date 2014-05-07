{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Bot (
    BotConfig (..)
,   BotState (..)
,   Bot
,   Component (..)
,   defaultBotConfig
,   withComponents
,   runBot
)   where

import Bot.Component
import Bot.Component.Impl.PingPong
import Bot.Component.Impl.Reboot
import Bot.IO

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import Data.Maybe
import Network
import Prelude hiding (catch)
import System.Exit
import System.Directory
import System.IO
import Text.Printf

-- | The configuration used to initialize an instance of a chat bot.
data BotConfig = BotConfig {
        cfgServer      :: String
    ,   cfgPort        :: Int
    ,   cfgData        :: String
    ,   cfgNick        :: String
    ,   cfgChannel     :: [String]
    ,   cfgComponents  :: [Bot Component]
    ,   cfgWriteRate   :: Double
}

-- | A default `BotConfig` containing `BotComponent`s that are likely wanted by
-- every bot. Such as pingResponse,
defaultBotConfig = BotConfig {
        cfgServer       = "localhost"
    ,   cfgPort         = 6667
    ,   cfgData         = "data"
    ,   cfgNick         = "IRCBot"
    ,   cfgChannel      = []
    ,   cfgComponents   = [
            pingPong
        ,   reboot
    ]
    ,   cfgWriteRate    = 5
}

-- | There are some components in the `defaultBotConfig` that probably should not
-- be taken out. This function exists to make the process of adding new
-- components easier:
--
-- > defaultBotConfig `withComponents` [
-- >        component1
-- >    ,   component2
-- > ]
withComponents  :: BotConfig -> [Bot Component] -> BotConfig
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
            createDirectoryIfMissing True cfgData
            socket      <-  connectTo cfgServer
                        $   PortNumber (fromIntegral cfgPort)
            chan        <-  atomically newTChan
            hSetBuffering socket NoBuffering
            return BotState {
                    socket
                ,   exitCode        = Nothing
                ,   botNick         = cfgNick
                ,   botHost         = ""
                ,   currentNick     = ""
                ,   currentChannel  = fromMaybe "" $ listToMaybe cfgChannel
                ,   dataDirectory   = cfgData
                -- We will update the components in the init method so that they
                -- can be evaluated within the Bot monad
                ,   components      = []
                ,   messageQueue    = chan
            }

        -- Close the socket and return with the desired exit code.
        disconnect BotState{..} = do
            hClose socket
            exitWith $ fromMaybe ExitSuccess exitCode

        -- Connect to the desired channels, and set up the nick
        init :: [Bot Component] -> Bot ()
        init cfgComponents = do
            handle    <- gets socket
            chan      <- gets messageQueue
            liftIO $ forkIO $ ircWriteLoop chan handle cfgWriteRate
            ircWrite "NICK" cfgNick
            ircWrite "USER" $ cfgNick ++ " 0 * :Greatest Guys bot"
            ircWrite "WHO" cfgNick
            mapM_ (ircWrite "JOIN") cfgChannel
            components  <-  sequence cfgComponents
            modify $ \s -> s { components }

        -- Grab messages off IRC and pass them to various `BotComponent`s
        loop :: Bot ()
        loop = do
            catch   runComponents
                $   \e  ->  liftIO (print (e:: SomeException))
                        >>  modify (\s -> s {exitCode = Just $ ExitFailure 1})
            -- Continue to loop if there is no exit code, other wise stop.
            liftM ((return <$>) . void . exitCode) get >>= fromMaybe loop

        -- Read a message from IRC and process it with each of the registered
        -- components
        runComponents :: Bot ()
        runComponents = do
            message     <-  ircReadTimeout 1000 -- wait for .1 seconds
            components  <-  gets components
                        >>= mapM (\c -> process message c `catch` handler c)
            modify $ \s -> s { components }
            -- Flush output so errors can actually be seen after they happen.
            lift $ hFlush stdout >> hFlush stderr
            where
                handler :: a -> SomeException -> Bot a
                handler a   =   (return a <*)
                            .   liftIO
                            .   putStrLn
                            .   ("ERROR: Component: " ++)
                            .   show

        -- Takes a message and a component and returns Bot wrapped resulting
        -- component. Used in runComponents.
        process :: String -> Component -> Bot Component
        process message (MkComponent (extractor, action)) = do
            newExtractor <- dropBot' extractor $ action message
            return $ MkComponent (newExtractor, action)

-- | Read from IRC, but if there is no response in a reasonable amount of time
-- return an empty string.
ircReadTimeout :: Int -> Bot String
ircReadTimeout timeout = do
    handle  <-  gets socket
    ready   <-  liftIO $ hWaitForInput handle timeout
    if ready then ircRead else return ""

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
