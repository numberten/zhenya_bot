{-# LANGUAGE RecordWildCards #-}
module Bot.Component.Impl.Grep (
    grep
)   where

import Bot.Component
import Bot.Component.Command
import Bot.Component.Impl.History
import Bot.Component.Impl.NickCluster
import Bot.IO

import Control.Applicative
import Control.Monad.Trans.Identity
import Data.List
import Text.Regex.TDFA
import qualified Data.Foldable as F
import qualified Data.Sequence as S

data GrepOptions = GrepOptions {
        context :: Int
    ,   matches :: Int
    ,   nick    :: String
    ,   pattern :: String
}

grep :: ClusterNickHandle -> HistoryHandle -> Bot Component
grep cluster history = mkComponentT $ commandT usage "!grep" action
    where
        -- The usage message, in case no arguments are passed.
        usage = UsageMessage ["usage: !grep [-c int] [-n nick] [-m matches] regex"]

        action :: [String] -> IdentityT Bot ()
        action args = do
            let GrepOptions{..} = parseArgs args
            -- ignore the last utterance for grepping
            history <-  (S.take <$> (\x -> x - 1) . S.length <*> id)
                    <$> getHistory history
            clusters    <-  liftBot $ aliasesForNick cluster nick
            let pred    =   and . flip ((<**>) . return) [
                            -- Optionally filter by nick
                            ((|| (nick == "")) . (`elem` clusters)) . fst
                            -- Filter previous greps
                        ,   not . isPrefixOf "!grep" . snd
                            -- Matches the given regular expression
                        ,   (=~ pattern) . snd
                        ]
            let found   =   reverse  -- display in chronological order
                        $   take matches
                        $   S.findIndicesR pred history
            liftBot $ case found of
                []      -> ircReply "Ain't no one ever said no such thing!"
                indices -> flip mapM_ indices $ \index -> do
                    let beginIndex  =   max 0 (index - context)
                    let lines       =   S.take (context * 2 + 1)
                                    $   S.drop beginIndex history
                    flip F.mapM_ lines $ \(nick, message) ->
                        ircReply $ concat ["<", nick, "> ", message]

        -- Parse flags out of arguments. Not going to win any awards for
        -- implementation here, but since there are only a couple flags,
        -- it's easier to enumerate the possible combinations than it is to
        -- import Parsec.
        parseArgs :: [String] -> GrepOptions
        parseArgs = parse "" "0" "1"

        parse :: String -> String -> String -> [String] -> GrepOptions
        parse _    ctx num ("-n":nick:xs) = parse nick ctx num xs
        parse nick _   num ("-c":ctx:xs)  = parse nick ctx num xs
        parse nick ctx _   ("-m":num:xs)  = parse nick ctx num xs
        parse nick ctx num xs             = makeOptions nick ctx num xs

        -- Helper function to parseArgs, converts raw strings and lists
        -- into final types for the GrepOptions record.
        makeOptions :: String
                    -> String
                    -> String
                    -> [String]
                    -> GrepOptions
        makeOptions nick context number rest = GrepOptions {
                nick    = nick
            ,   pattern = if null rest then "." else unwords rest
                -- These will fail if bad, but we are sandboxed, so it's
                -- whatever.
            ,   context = read context
            ,   matches = read number
            }
