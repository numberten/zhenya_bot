module Bot.Component.Impl.Substitute (
    substitute
)   where

import Bot.Component
import Bot.Component.Conditional
import Bot.Component.Impl.History
import Bot.Component.Impl.NickCluster
import Bot.IO

import Control.Monad.State
import Control.Monad.Trans.Identity
import Data.List (isPrefixOf)
import Data.List.Split (splitWhen)
import Data.List.Utils (replace, subIndex)
import qualified Data.Sequence as S

substitute :: ClusterNickHandle -> HistoryHandle -> Bot Component
substitute cluster history = mkComponentT $ conditionalT ("s/" `isPrefixOf`) action
  where
    action :: String -> IdentityT Bot ()
    action args = do
        nick <- gets currentNick
        history <- getHistory history
        cluster <- liftBot $ aliasesForNick cluster nick
        case splitter args of
            (_:find:replace:"g":_)  ->
                liftBot . ircReply 
                        . ((nick++": ")++)
                        . suball (removeBackSlash find) replace
                        . snd 
                        . flip S.index 1 
                        . S.filter ((`elem` cluster) . fst) 
                        $ S.reverse history
            (_:find:replace:_)      -> 
                liftBot . ircReply 
                        . ((nick++": ")++)
                        . subfirst (removeBackSlash find) replace
                        . snd 
                        . flip S.index 1 
                        . S.filter ((`elem` cluster) . fst) 
                        $ S.reverse history
            _                       -> return ()

suball :: String -> String -> String -> String
suball = safe_replace

subfirst :: String -> String -> String -> String
subfirst foo bar string = 
  case subIndex foo string of
    Nothing -> string
    Just i  -> take i string
            ++ bar
            ++ drop (i + length foo) string

splitter :: String -> [String]
splitter = (map . map) snd 
         . splitWhen isNotEscapedForwardSlash
         . (zip `ap` tail)
  where
    isNotEscapedForwardSlash (x,y) =
      if (x /= '\\' && y == '/')
        then True
        else False

removeBackSlash :: String -> String
removeBackSlash ""             = ""
removeBackSlash ('\\':'\\':xs) = '\\':removeBackSlash xs
removeBackSlash ('\\':xs)      = removeBackSlash xs
removeBackSlash (x:xs)         = x:removeBackSlash xs

safe_replace :: String -> String -> String -> String
safe_replace a b c = if a == "" then c else replace a b c
