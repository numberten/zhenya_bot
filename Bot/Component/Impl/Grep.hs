{-# LANGUAGE RecordWildCards #-}
module Bot.Component.Impl.Grep (
    grep
)   where

import Bot.Component
import Bot.Component.Command
import Bot.Component.Impl.History
import Bot.IO

import Control.Applicative
import Control.Monad.Trans.Identity
import Text.Regex.TDFA
import qualified Data.Foldable as F
import qualified Data.Sequence as S

data GrepOptions = GrepOptions {
        context :: Int
    ,   nick    :: String
    ,   pattern :: String
}

grep :: HistoryHandle -> Bot Component
grep handle = mkComponentT $ commandT "!grep" action
    where
        action :: [String] -> IdentityT Bot ()
        action []   = liftBot $ ircReply "!grep [-c int] [-n nick] regex"
        action args = do
            let GrepOptions{..} = parseArgs args
            -- ignore the last utterance for grepping
            history <-  (S.take <$> (\x -> x - 1) . S.length <*> id)
                    <$> getHistory handle
            let pred    =   (&&)
                        <$> (=~ pattern) . snd
                        <*> ((|| (nick == "")) . (== nick)) . fst
            liftBot $ case S.findIndexR pred history of
                Nothing     -> ircReply "Ain't no one ever said no such thing!"
                Just index  -> do
                    let beginIndex  =   max 0 (index - context)
                    let lines       =   S.take (context * 2 + 1)
                                    $   S.drop beginIndex history
                    flip F.mapM_ lines $ \(nick, message) ->
                        ircReply $ concat ["<", nick, "> ", message]

        -- Parse flags out of arguments. Not going to win any awards for
        -- implementation here, but since there are only two flags, it's easier
        -- to enumerate the possible combinations than it is to import Parsec.
        parseArgs :: [String] -> GrepOptions
        parseArgs ("-n":nick:"-c":context:rest) = makeOptions nick context rest
        parseArgs ("-c":context:"-n":nick:rest) = makeOptions nick context rest
        parseArgs ("-n":nick:rest)              = makeOptions nick "0" rest
        parseArgs ("-c":context:rest)           = makeOptions "" context rest
        parseArgs rest                          = makeOptions "" "0" rest

        -- Helper function to parseArgs, converts raw strings and lists into
        -- final types for the GrepOptions record.
        makeOptions :: String -> String -> [String] -> GrepOptions
        makeOptions nick context rest = GrepOptions {
                context = read context -- will fail if bad, but we are sandboxed
            ,   nick    = nick
            ,   pattern = if null rest then "." else unwords rest
            }
