{-# OPTIONS_GHC -fno-cse #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
import Bot

import System.Console.CmdArgs.Implicit

-- | A data type defining the various command line arguments that may be
-- supplied.
data Flags = Flags {
        serverFlag  ::  String
    ,   portFlag    ::  Int
    ,   nickFlag    ::  String
    ,   channelFlag ::  String      
} deriving (Data, Typeable, Show)

-- | Declare the default values, help strings and options values for each of the
-- different flags.
flagDefinition = Flags {
        serverFlag  =   "crafting.dangerbear.in"
                    &=  help "The IRC server to connect to"
                    &=  opt "server"
    ,   portFlag    =   6667
                    &=  help "The port on which the IRC server is running"
                    &=  opt "port"
    ,   nickFlag    =   "zhenya_bot"
                    &=  help "The nick that the bot should take"
                    &=  opt "nick"
    ,   channelFlag =   "#greatestguys"
                    &=  help "The channel that the bot should join"
                    &=  opt "channel"
}

-- | Grab configuration from the command line, attach appropriate BotComponents
-- and start the IRC bot.
main :: IO ()
main = do
    flags@Flags{..} <-  cmdArgs flagDefinition
    runBot $ defaultBotConfig {
            cfgServer   = serverFlag
        ,   cfgPort     = portFlag
        ,   cfgChannel  = channelFlag
        ,   cfgNick     = nickFlag
        } `withComponents` [
        ]

