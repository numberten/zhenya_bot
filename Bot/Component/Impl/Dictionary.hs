module Bot.Component.Impl.Dictionary (
    define
)   where

import Bot.Component
import Bot.Component.Command
import Bot.IO

import Data.Char
import Data.List
import Data.ByteString.Lazy.Internal as LBS
import Network.HTTP.Conduit
import Text.HTML.TagSoup

lookupPageUrl = "http://www.urbandictionary.com/define.php?term="
randomPageUrl = "http://www.urbandictionary.com/random.php"

define :: Bot Component
define = command usage "!define" defineAction
    where
        -- The usage message, in case no arguments are passed.
        usage = UsageMessage ["usage: !define word"]

        -- Looks up the definition of words and reports it to chat.
        defineAction words = do
            tags        <-  liftIO
                        $   return
                        .   parseTags
                        .   show
                        =<< (simpleHttp 
                        $   lookupPageUrl
                        ++  map (\x -> if x == ' ' then '+' else x)
                                (unwords words) :: IO LBS.ByteString)

            randomtags  <-  liftIO
                        $   return
                        .   parseTags
                        .   show
                        =<< (simpleHttp randomPageUrl :: IO LBS.ByteString)

            let targetWord  =   unwords words
            let randWord    =   getWord randomtags
            let queriedDef  =   getDefinition tags
            let randDef     =   getDefinition randomtags

            -- If we use the random definition then replace the defined word
            -- with the query.
            let (def, rand) =   if null queriedDef
                                    then (randDef, True)
                                    else (queriedDef, False)

            let replaceWord =   replace randWord targetWord

            let definition  =   (if rand then replaceWord else id) def

            ircReply        $   targetWord ++ ": " ++ definition

        -- Trims white space from the front and back of a string.
        trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

        -- Squashes double white space into a single space.
        squash []           = []
        squash (' ':' ':xs) = squash (' ':xs)
        squash (x:xs)       = x : squash xs

        getDefinition = getDivText "meaning"

        getWord = getDivText "word"

        getDivText divClass = trim . squash
                            . filterBackSlashNR
                            . concatMap fromTagText
                            . filter isTagText
                            . takeWhile (~/= "</div>")
                            . dropWhile (~/= divOpen)
          where divOpen = "<div class=\"" ++ divClass ++ "\">"

        -- Performs a case insensitive replacement.
        replace :: String -> String -> String -> String
        replace _   _   []      = []

        replace old new source
            | isPrefixOf (map toLower old) (map toLower source)
            = new ++ replace old new (drop (length old) source)

        replace old new (x:xs)  = x : replace old new xs

        filterBackSlashNR :: String -> String
        filterBackSlashNR = reverse . fbsn ""
          where
            fbsn acc ""            = acc
            fbsn acc ('\\':'n':s) = fbsn acc s
            fbsn acc ('\\':'r':s) = fbsn acc s
            fbsn acc (s:ss)        = fbsn (s:acc) ss


