module Bot.Component.Impl.Youtube (
    youtube
)   where

import Bot.Component
import Bot.Component.Regex
import Bot.IO

import Control.Monad
import Data.List (isPrefixOf)
import Network.Curl.Download
import Text.HTML.TagSoup

youtube :: Bot Component
youtube = regex pattern linkHandler
    where
        -- Matches youtube and shortened youtube urls.
        pattern         = "youtube.com/watch\\?([a-zA-Z]+=[a-zA-Z0-9_#-]+&)*v=[a-zA-Z0-9_#-]+|youtu.be/[a-zA-Z0-9_#-]+"

        -- Run for each link that looks like a youtube video
        linkHandler url =   (if "youtu." `isPrefixOf` url
                            then liftIO (openAsTags ("http://www.youtube.com/watch?v=" ++ drop 1 (dropWhile (/= '/') url) ++ "&feature=youtu.be"))
                            else liftIO (openAsTags ("http://www." ++ url)))
                        >>= either (const $ return ()) describeLink

        -- Attempt to pull the title of the video and relay it to IRC
        describeLink    =   mapM_ (ircReply . soupToTitle)
                        .   sections (~== "<span id=\"eow-title\" title=\"\">")

        -- Extract the title text from a bunch of tags
        soupToTitle     =   concat
                        .   liftM (fromAttrib "title")
                        .   take 1
