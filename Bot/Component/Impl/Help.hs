{-# LANGUAGE RecordWildCards #-}
module Bot.Component.Impl.Help (
    helpComponent
)   where

import Bot.Component
import Bot.Component.Command
import Bot.IO

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Maybe

helpComponent :: Bot Component
helpComponent = command "!help" action
    where
        action :: [String] -> Bot ()
        -- Unfortunately we cannot use help information for !help's help message
        -- since the list of all known components is not known when the
        -- component is first constructed.
        action []           = showHelpString =<< helpHelpString
        action ("help":_)   = showHelpString =<< helpHelpString
        action ("!help":_)  = showHelpString =<< helpHelpString

        action (component:_)    =   helpStringForComponent component
                                >>= showHelpString

        showHelpString :: [String] -> Bot ()
        showHelpString []       = return ()
        showHelpString ("":xs)  = ircReply " " >> showHelpString xs
        showHelpString (x:xs)   = ircReply x >> showHelpString xs

        -- Given an aliases, look up the corresponding help string.
        helpStringForComponent :: String -> Bot [String]
        helpStringForComponent name = do
            components <- mapMaybe getHelp <$> gets components
            return $ join $ take 1 $ mapMaybe (messageToString name) components

        getHelp :: Component -> Maybe HelpMessage
        getHelp (MkComponent _ help) = help

        -- Given a target alias and a HelpMessage, return just the help string
        -- if the message matches, or nothing otherwise.
        messageToString :: String -> HelpMessage -> Maybe [String]
        messageToString alias HelpMessage{..} =
            guard (alias `elem` helpAliases) >> Just helpString

        -- The help string for the help component. Because the message
        -- references other components, it must be wrapped in Bot.
        helpHelpString :: Bot [String]
        helpHelpString = do
            names <- map canonicalName <$> mapMaybe getHelp <$> gets components
            return $ [
                    "usage: !help [command]"
                ,   ""
                ,   "   The !help command is used to display usage and other"
                ,   "general information about a component. When no argument is"
                ,   "given, this message will be displayed."
                ,   ""
                ,   "Components with help pages:"
                ] ++ formatNames names

        -- Given a list of canonical names, attempt to fit as many as possible
        -- onto lines of length 50.
        formatNames :: [String] -> [String]
        formatNames names = map (padding ++) $ formatNames' "" names

        formatNames' :: String -> [String] -> [String]
        formatNames' ""     []           = []
        formatNames' line   []           = [line]
        formatNames' ""     (name:names) = formatNames' name names
        formatNames' line   allNames@(name:names)
            | okLength line name    = formatNames' (line ++ ", " ++ name) names
            | otherwise             = (line ++ ",") : formatNames' "" allNames

        maxLineLength = 60

        padding = "    "

        okLength line name = length line + 2 + length name < maxLineLength
