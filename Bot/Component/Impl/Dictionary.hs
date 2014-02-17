module Bot.Component.Impl.Dictionary (
    define
)   where

import Bot.Component
import Bot.Component.Command
import Bot.IO

import Network.HTTP
import Text.HTML.TagSoup

define :: Bot Component
define = command "!define" defineAction
    where
        -- Looks up the definition of words and reports it to chat
        defineAction words = do
            tags        <-  liftIO
                        .   fmap parseTags
                        $   getResponseBody
                        =<< simpleHTTP (getRequest
                        $   "http://www.urbandictionary.com/define.php?term="
                        ++  map (\x -> if x == ' ' then '+' else x)
                                (unwords words))
            redirect    <-  liftIO
                        .   fmap parseTags
                        $   getResponseBody
                        =<< simpleHTTP (getRequest "http://www.urbandictionary.com/random.php")

            let randomUrl   = fromAttrib "href" . head $ drop 3 redirect

            randomtags  <-  liftIO
                        .   fmap parseTags
                        $   getResponseBody
                        =<< simpleHTTP (getRequest randomUrl)

            let isntNewline = \x -> if x == '\n' then False else True

            let queriedDef  =   drop 1 
                            .   dropWhile (~/= "<div class=\"meaning\">") 
                            $   tags

            let randDef     =   drop 1 
                            .   dropWhile (~/= "<div class=\"meaning\">") 
                            $   randomtags

            let def = if (queriedDef == []) then randDef else queriedDef

            let definition  =   filter isntNewline
                            .   fromTagText
                            .   head
                            $   def

            ircReply        $   unwords words ++ ": " ++ definition

