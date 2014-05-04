module Bot.Component.Impl.Dictionary (
    define
)   where

import Bot.Component
import Bot.Component.Command
import Bot.IO

import Data.Char
import Data.List
import Network.HTTP
import Text.HTML.TagSoup

lookupPageUrl = "http://www.urbandictionary.com/define.php?term="
randomPageUrl = "http://www.urbandictionary.com/random.php"

define :: Bot Component
define = command "!define" defineAction
    where
        -- Looks up the definition of words and reports it to chat
        defineAction words = do
            tags        <-  liftIO
                        .   fmap parseTags
                        $   getResponseBody
                        =<< simpleHTTP (getRequest
                        $   lookupPageUrl
                        ++  map (\x -> if x == ' ' then '+' else x)
                                (unwords words))

            redirect    <-  liftIO
                        .   fmap parseTags
                        $   getResponseBody
                        =<< simpleHTTP (getRequest randomPageUrl)

            let randomUrl   = fromAttrib "href" . head $ drop 3 redirect

            randomtags  <-  liftIO
                        .   fmap parseTags
                        $   getResponseBody
                        =<< simpleHTTP (getRequest randomUrl)

            let queriedDef  =   drop 1
                            .   dropWhile (~/= "<div class=\"meaning\">")
                            $   tags

            let targetWord  =   unwords words

            let randWord    =   fromTagText
                            .   head
                            .   drop 3  -- skip the div and a tags
                            .   dropWhile (~/= "<div class=\"word\">")
                            $   randomtags

            let randDef     =   drop 1
                            .   dropWhile (~/= "<div class=\"meaning\">")
                            $   randomtags

            let (def, rand) =   if (queriedDef == [])
                                    then (randDef, True)
                                    else (queriedDef, False)

            let replaceWord =   replace randWord targetWord

            let definition  =   (if rand then replaceWord else id)
                            .   filter (/= '\n')
                            .   fromTagText
                            .   head
                            $   def

            ircReply        $   targetWord ++ ": " ++ definition

        -- Performs a case insensitive replacement.
        replace :: String -> String -> String -> String
        replace _   _   []      = []

        replace old new source
            | isPrefixOf (map toLower old) (map toLower source)
            = new ++ replace old new (drop (length old) source)

        replace old new (x:xs)  = x : replace old new xs
