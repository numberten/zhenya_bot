module Bot.Component.Impl.Youtube (
    youtube
)   where

import Bot.Component
import Bot.Component.Regex
import Bot.IO

import Control.Monad
import Network.Curl.Download
import Text.HTML.TagSoup

youtube :: Bot BotComponent
youtube = regex pattern linkHandler
    where
        pattern         = "youtube.com/watch\\?v=[a-zA-Z0-9]*"

        -- Run for each link that looks like a youtube video
        linkHandler url =   liftIO (openAsTags ("http://www." ++ url))
                        >>= either (const $ return ()) describeLink

        -- Attempt to pull the title of the video and relay it to IRC
        describeLink    =   mapM_ (ircReply . soupToTitle)
                        .   sections (~== "<span id=\"eow-title\">")

        -- Extract the title text from a bunch of tags
        soupToTitle     =   concat
                        .   liftM (fromAttrib "title")
                        .   take 1
