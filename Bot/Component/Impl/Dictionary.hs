module Bot.Component.Impl.Dictionary (
    define
)   where

import Bot.Component
import Bot.Component.Command
import Bot.IO

import Control.Monad
import Network.Curl.Download
import Text.HTML.TagSoup

define :: Bot BotComponent
define = command "!define" (defineAction . concat . take 1)
    where
        -- Action that looks up definition of word
        defineAction word   =   liftIO (openAsTags ("http://www.urbandictionary.com/define.php?term=" ++ word))
                            >>= either (const $ return ()) reportDefinition

        -- Attempt to pull the definition of the word and relay it to IRC
        reportDefinition     =   mapM_ (ircReply . soupToDefinition)
                            .   sections (~== "<div class=\"definition\">")

        -- Extract the definition text from a bunch of tags
        soupToDefinition    =   concat
                            .   liftM (fromTagText)
                            .   take 1
