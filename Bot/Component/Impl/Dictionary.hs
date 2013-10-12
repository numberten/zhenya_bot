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
            randomUrl   <-  liftIO
                        .   fmap parseTags
                        $   getResponseBody
                        =<< simpleHTTP (getRequest "http://www.urbandictionary.com/random.php")
            let definitionTags  = drop 1 $ dropWhile (~/= "<div class=\"definition\">") tags
            let randomUrlStr    = fromAttrib "href" . head $ drop 3 randomUrl
            randomTags  <- liftIO
                        . fmap parseTags
                        $ getResponseBody
                        =<< simpleHTTP (getRequest randomUrlStr)
            let randomDefTags   = drop 1 $ dropWhile (~/= "<div class=\"definition\">") randomTags
            let definition = if lootTagTexts definitionTags == "" 
                                then lootTagTexts randomDefTags 
                                else lootTagTexts definitionTags
            ircReply    $ unwords words ++ ": " ++ definition
        lootTagTexts tags = fst $ foldl f ("", 0) tags
            where
                f (str, i) tag  | i < 0             = (str, i)
                                | isTagText tag     = (str ++ fromTagText tag, i)
                                | isTagOpen tag     = (str, i+1)
                                | isTagClose tag    = (str, i-1)
                                | otherwise         = error "This should never happen!"



