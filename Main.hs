{-# OPTIONS_GHC -fno-cse #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
import Bot
import Bot.Component.Command
import Bot.Component.Conditional
import Bot.Component.Fuzzy
import Bot.Component.Impl.Dictionary
import Bot.Component.Impl.FileSearch
import Bot.Component.Impl.Github
import Bot.Component.Impl.Goodbye
import Bot.Component.Impl.Grep
import Bot.Component.Impl.History
import Bot.Component.Impl.Lists
import Bot.Component.Impl.NGram
import Bot.Component.Impl.NickCluster
import Bot.Component.Impl.Op
import Bot.Component.Impl.Queens
import Bot.Component.Impl.Roll
import Bot.Component.Impl.Uptime
import Bot.Component.Impl.Seen
import Bot.Component.Impl.Spotify
import Bot.Component.Impl.Stalker
import Bot.Component.Impl.Youtube
import Bot.IO

import Data.List
import System.Console.CmdArgs.Implicit

-- | A data type defining the various command line arguments that may be
-- supplied.
data Flags = Flags {
        serverFlag  ::  String
    ,   portFlag    ::  Int
    ,   nickFlag    ::  String
    ,   channelFlag ::  [String]
    ,   dataFlag    ::  FilePath
    ,   writeFlag   ::  Double
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
    ,   nickFlag    =   "zhenya_bot"
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
    ,   writeFlag   =   5  -- One second
                    &=  explicit
                    &=  help "Number of IRC writes per second."
                    &=  name "write-rate"
                    &=  typ "Double"
}   &=  summary "Greatest guys IRC bot"
    &=  program "zhenya_bot"

-- | Grab configuration from the command line, attach appropriate BotComponents
-- and start the IRC bot.
main :: IO ()
main = do
    Flags{..}     <-  cmdArgs flagDefinition
    cnHandle      <-  newClusterNickHandle
    histHandle    <-  newHistoryHandle
    stalkerHandle <-  newStalkerHandle
    -- Create a directory for runtime data if one does not already exist
    runBot $ defaultBotConfig {
            cfgServer     = serverFlag
        ,   cfgPort       = portFlag
        ,   cfgData       = dataFlag
        ,   cfgChannel    = nub channelFlag
        ,   cfgNick       = nickFlag
        ,   cfgWriteRate  = writeFlag
        } `withComponents` [
            clusterNickService cnHandle 0.3
        ,   historyService histHandle
        ,   stalker stalkerHandle

        ,   define
        ,   fileSearch
        ,   imitate cnHandle
        ,   github
        ,   grantOps stalkerHandle
        ,   grep cnHandle histHandle
        ,   lists
        ,   calcQueens
        ,   rollDice
        ,   sayGoodbye
        ,   seen cnHandle
        ,   spotify
        ,   uptime
        ,   youtube

        ,   command (UsageMessage ["usage: !id string"]) "!id"
                (ircReply . unwords)

        ,   conditional (nickFlag `isPrefixOf`) (ircReply "hm?")

        ,   fuzzyMatch "pigups" 0.1
                (ircReply "Jolly good pigups, jolly good.")

        ,   fuzzyMatch "you can't dry a bug!" 0.2
                (ircReply "Noted.")

        ,   fuzzyMatch "cheap meat" 0.1
                (ircReply "Can we go to DQ?")

        ,   fuzzyMatch "I'd expect it." 0.1
                (ircReply "me too")

        ,   fuzzyMatch "Worse than child birth." 0.2
                (ircReply "Check your privilege!")
        ]
