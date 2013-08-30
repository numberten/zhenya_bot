module Bot.Component.Impl.Dictionary (
    define
)   where

import Bot.Component
import Bot.Component.Command
import Bot.IO

import Control.Monad
import Network.Curl.Download
import Text.HTML.TagSoup
import Text.HTML.Download

define :: Bot BotComponent
define = command "!define" (defineAction)
    where
        defineAction words = do
            let word    = concat
                        . take 1
                        $ words
            tags <- liftIO $ fmap parseTags $ openURL $ "http://www.urbandictionary.com/define.php?term=" ++ word
            let definition = fromTagText (dropWhile (~/= "<div class=\"definition\">") tags !! 1)
            ircReply definition

