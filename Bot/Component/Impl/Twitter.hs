module Bot.Component.Impl.Twitter (
    twitter
)   where

import Bot.Component
import Bot.Component.Regex
import Bot.IO

import Control.Applicative
import Control.Monad
import Network.Curl.Download
import Text.HTML.TagSoup
import qualified Data.ByteString.Char8 as BS

twitter :: Bot Component
twitter = regex pattern linkHandler
    where
        pattern     = "twitter.com/[^ ]+/status(es)?/[0-9]+"

        userNameClass = "username js-action-profile-name"
        fullNameClass = "fullname js-action-profile-name show-popup-with-id"
        statusClass   = "js-tweet-text tweet-text"

        downloadAsTags url  =   fmap parseTags
                            <$> openURIWithOpts [] url

        -- Run for each link that looks like a spotify track video
        linkHandler url =   liftIO (downloadAsTags ("https://" ++ url))
                        >>= either (const $ ircReply "shit") describeLink

        -- Attempt to pull out the tweet from the soup.
        describeLink = ircReply . concat . (<**> [
                              soupToFullName
                          ,   const " ("
                          ,   soupToUserName
                          ,   const ") "
                          ,   soupToStatus
                          ]) . return

        soupToStatus  = getInnerTextOf "p" statusClass

        soupToFullName  = getInnerTextOf "strong" fullNameClass

        soupToUserName  = getInnerTextOf "span" userNameClass

        getInnerText  = BS.unpack -- jk, we actually just want a String
                      . BS.concat -- we have a [BS], we want a BS
                      . liftM fromTagText -- extract text from the node
                      . filter (isTagText) -- grab all of the text nodes

        getInnerTextOf tag clazz  = getInnerText
                                  . takeWhile (~/= closeTag)
                                  . concat
                                  . take 1
                                  . sections (~== openTag)
            where
              closeTag  = "</" ++ tag ++ ">"
              openTag   = "<" ++ tag ++ " class='" ++ clazz ++ "'>"
