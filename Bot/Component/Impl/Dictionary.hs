module Bot.Component.Impl.Dictionary (
    define
)   where

import Bot.Component
import Bot.Component.Command
import Bot.IO

import Control.Monad
import Network.Curl.Download
import Network.HTTP
import Text.HTML.TagSoup
import Text.HTML.Download

define :: Bot BotComponent
define = command "!define" defineAction
    where
        -- Looks up the definition of words and reports it to chat
        defineAction words = do
            tags        <-  liftIO 
                        .   fmap parseTags 
                        $   getResponseBody 
                        =<< simpleHTTP (getRequest 
                        $   "http://www.urbandictionary.com/define.php?term="
                        ++  (map (\x -> if x == ' ' then '+' else x)
                        $   unwords words))
            randomtags  <-  liftIO
                        .   fmap parseTags
                        $   getResponseBody
                        =<< simpleHTTP (getRequest
                        $   "http://www.urbandictionary.com/random.php"
                        ++  (map (\x -> if x == ' ' then '+' else x)
                        $   unwords words))
            let definitionTags          = drop 1 $ dropWhile (~/= "<div class=\"definition\">") tags
            let randomDefinitionTags    = drop 1 $ dropWhile (~/= "<div class=\"definition\">") randomtags
            ircReply    $ unwords words ++ ": " ++ lootTagTexts definitionTags ++"#"
            ircReply    $ unwords words ++ ": " ++ lootTagTexts randomDefinitionTags ++"#"

        lootTagTexts tags = fst $ foldl f ("", 0) tags
            where
                f (str, i) tag  | i < 0             = (str, i)
                                | isTagText tag     = (str ++ fromTagText tag, i)
                                | isTagOpen tag     = (str, i+1)
                                | isTagClose tag    = (str, i-1)
                                | otherwise         = error "This should never happen!"



