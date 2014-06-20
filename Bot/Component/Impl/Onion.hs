module Bot.Component.Impl.Onion (
    onion
)   where

import Bot.Component
import Bot.Component.Regex
import Bot.IO

import Control.Applicative
import Control.Monad
import Network.Curl.Download
import Network.Curl.Opts
import Text.HTML.TagSoup
import qualified Data.ByteString.Char8 as BS

onion :: Bot Component
onion = regex pattern linkHandler
    where
        pattern     = "www.theonion.com/articles/[a-zA-Z0-9_#-]+,[0-9]+"
        userAgent   = CurlUserAgent "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)"

        downloadAsTags url  =   fmap parseTags
                            <$> openURIWithOpts [userAgent] url

        -- Run for each link that looks like an onion article
        linkHandler url =   liftIO (downloadAsTags ("http://" ++ url))
                        >>= either (const $ ircReply "uh oh") describeLink

        -- Attempt to pull the title of the article and relay it to IRC
        describeLink    =   mapM_ ( ircReply 
                                  . ("Breaking News: "++) 
                                  . soupToTitle
                                  )
                        .   sections (~== "<title>")

        -- Extract the title text from a bunch of tags
        soupToTitle     =   takeWhile (/= '|') -- take only the title part
                        .   BS.unpack -- jk, we actually just want a String
                        .   BS.concat -- we have a [BS], we want a BS
                        .   liftM fromTagText -- extract text from the node
                        .   take 1 -- sections gives all after, only want first
                        .   filter (isTagText) -- grab the text node of <title>

