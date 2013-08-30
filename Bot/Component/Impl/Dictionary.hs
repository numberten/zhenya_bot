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
            let definition = fromTagText (dropWhile (~/= "<div class=\"definition\">") tags !! 1)
            ircReply    $ unwords words ++ ": " ++ definition

