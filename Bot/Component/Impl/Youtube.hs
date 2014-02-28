module Bot.Component.Impl.Youtube (
    youtube
)   where

import Bot.Component
import Bot.Component.Regex
import Bot.IO

import Control.Monad
import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import Network.Curl
import Network.Curl.Opts
import Text.HTML.TagSoup
import Text.Regex

youtube :: Bot Component
youtube = regex pattern linkHandler
    where
        -- | Matches youtube and shortened youtube urls.
        pattern         = "youtube.com/watch\\?([a-zA-Z]+=[a-zA-Z0-9_#-]+&)*v=[a-zA-Z0-9_#-]+|youtu.be/[a-zA-Z0-9_#-]+"

        linkHandler url =   (if "youtu." `isPrefixOf` url
                            then liftIO (scrape ("http://m.youtube.com/watch?v=" ++ drop 1 (dropWhile (/= '/') url) ++ "&feature=youtu.be"))
                            else liftIO (scrape ("http://m." ++ url)))
                        >>= ircReply 

-- | Helper function that scrapes mobile youtube
-- and returns the title and length of the url
-- passed to it.
scrape url = do
    let useragent   =   CurlUserAgent "NOKIA"
    (_,html)        <-  curlGetString url
                    $   [useragent]
    let tags        =   parseTags html
    let title       =   tagToTitle
                    .   head
                    .   tail
                    .   dropWhile (not . isTagOpenName "title")
                    $   tags
    let time        =   tagToTime
                    .   head
                    .   filter (isJust . match . fromTagText)
                    .   filter isTagText
                    $   tags
    return          $   title ++ time
    where
        -- | Turns a TagText containing a time, into a properly
        -- formatted string.
        tagToTime  tag  =   '(' : init (tail (fromTagText tag)) ++ ")"
        -- | Turns a TagText containing a title, into a properly
        -- formatted string.
        tagToTitle tag  =   reverse
                        .   tail
                        .   dropWhile (/='-')
                        .   reverse
                        $   fromTagText tag
        -- | Regex for matching a video duration.
        match           =   matchRegex 
                        $   mkRegex "^\n[0-9]+:[0-9]+\n$"

