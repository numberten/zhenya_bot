{-# OPTIONS_GHC -fno-cse #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
import Bot
import Bot.Component.Command
import Bot.Component.Conditional
import Bot.Component.Fuzzy
import Bot.Component.Impl.Goodbye
import Bot.Component.Impl.Op
import Bot.Component.Impl.Roll
import Bot.Component.Impl.Uptime
import Bot.Component.Impl.Seen
import Bot.Component.Impl.Loop
import Bot.IO

import Data.List
import System.Console.CmdArgs.Implicit
import System.Directory

-- | A data type defining the various command line arguments that may be
-- supplied.
data Flags = Flags {
        serverFlag  ::  String
    ,   portFlag    ::  Int
    ,   nickFlag    ::  String
    ,   channelFlag ::  [String]
    ,   dataFlag    ::  FilePath
} deriving (Data, Typeable, Show)

-- | Declare the default values, help strings and options values for each of the
-- different flags.
flagDefinition = Flags {
        serverFlag  =   "crafting.dangerbear.in"
                    &=  explicit 
                    &=  help "The IRC server to connect to"
                    &=  name "server"
                    &=  typ "HostName"
    ,   portFlag    =   6667
                    &=  explicit 
                    &=  help "The port on which the IRC server is running"
                    &=  name "port"
                    &=  typ "PortNumber"
    ,   nickFlag    =   "zhenya_bot_test"
                    &=  explicit 
                    &=  help "The nick that the bot should take"
                    &=  name "nick"
                    &=  typ "Nick"
    ,   channelFlag =   []
                    &=  explicit 
                    &=  help "The channel that the bot should join"
                    &=  name "channel"
                    &=  typ "Channel"
    ,   dataFlag    =   "data"
                    &=  explicit 
                    &=  help "The directory persistent files should be stored"
                    &=  name "data"
                    &=  typ "Directory"
}   &=  summary "Greatest guys IRC bot"
    &=  program "zhenya_bot"

-- | Grab configuration from the command line, attach appropriate BotComponents
-- and start the IRC bot.
main :: IO ()
main = do
    Flags{..} <-  cmdArgs flagDefinition
    -- Create a directory for runtime data if one does not already exist
    runBot $ defaultBotConfig {
            cfgServer   = serverFlag
        ,   cfgPort     = portFlag
        ,   cfgData     = dataFlag
        ,   cfgChannel  = nub channelFlag
        ,   cfgNick     = nickFlag
        } `withComponents` [
            grantOps
        ,   rollDice
        ,   sayGoodbye
        ,   seen
        ,   uptime
        ,   baconLoop

        ,   command "!id" (ircReply . unwords)

        ,   conditional (nickFlag `isPrefixOf`) (ircReply "hm?") 

        ,   fuzzyMatch "pigups" 0.1 
                (ircReply "Jolly good pigups, jolly good.") 

        ,   fuzzyMatch "you can't dry a bug!" 0.2
                (ircReply "Noted.") 

        ,   fuzzyMatch "cheap meat" 0.2 
                (ircReply "Can we go to DQ?") 

        ,   fuzzyMatch "I'd expect it." 0.2
                (ircReply "me too")
        ]

