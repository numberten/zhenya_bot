{-#LANGUAGE TupleSections#-}
module Bot.Component.Impl.NGram (
    imitate
)   where

import Bot.Component
import Bot.Component.Combinator
import Bot.Component.Command
import Bot.Component.Impl.NickCluster
import Bot.Component.Stateful
import Bot.IO

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Data.Char
import Data.List
import Data.Maybe
import Data.Random
import Data.Random.Distribution.Uniform
import Data.Random.Distribution.Categorical
import Data.Random.Sample
import Data.Ratio
import NLP.Tokenize
import System.FilePath
import qualified Data.Map as M
import qualified Data.Sequence as S

type Name = String
type Token = String
type BiGram = (Gram, Gram)
type BiGramModel r = M.Map Gram (r Gram)

data Gram = StartGram | EndGram | TokenGram Token
    deriving (Show, Eq, Ord, Read)

type ImitateState = M.Map String (BiGramModel IO)

-- | Speak like the various members of the IRC channel.
imitate :: ClusterNickHandle -> Bot BotComponent
imitate handle = stateful commandAction initialState
    where
        -- Where should we look for the catalogue?
        logPath = do
            directory <- gets dataDirectory
            return (directory </> "ngram.txt")

        initialState :: Bot ImitateState
        initialState = do
            nickClusters    <-  allNickAliases handle
            log             <-  logPath >>= liftIO . readFile
            let clusterMaps =   map (M.fromList . map (,())) nickClusters
            let logLines    =   lines $ map toLower $ log
            let allMessages =   (groupByName $ map processLine logLines) 
            return $ M.map (createModel . concatMap bigrams) allMessages
            --let nickToLines =   map (M.intersection allMessages) clusterMaps
            --let models      =   map (createModel . concatMap bigrams) 
            --                $   concatMap M.elems nickToLines
            --return $ M.fromList $ zip (map (head . M.keys) nickToLines) models

        -- Returns the nick of the same cluster as the one given that is used as
        -- the key for the map of nicks to models.
        canonicalNick :: String -> StateT ImitateState Bot (Maybe String)
        canonicalNick nick = do
            modelMap    <-  get
            nicks       <-  lift $ aliasesForNick handle nick
            return $ listToMaybe $ filter (`elem` nicks) $ M.keys modelMap

        commandAction = commandT "!be" $ \args -> case args of
            [nick]  -> do
                keyNick <- canonicalNick nick
                modelMap    <-  get
                p           <- lift $ logPath
                fromMaybe (lift $ ircReply "not a guy.") $ keyNick >>= \keyNick -> return $ do
                    model   <- gets (M.! keyNick)
                    message <- liftIO $ utterance model
                    lift $ ircReply message
            _       -> lift $ ircReply "be who?"

-- | Process a line from the specially formatted log file.
processLine :: String -> (Name, [Token])
processLine line = (name, tokenize message)
    where 
        tabIndex = fromJust $ elemIndex '\t' line
        (name, '\t':message) = splitAt tabIndex line

-- | Group each line by the nick that said it.
groupByName :: [(Name, a)] -> M.Map Name [a]
groupByName = M.fromListWith (++) . map (second return)

-- | Convert a Gram into the corresponding String.
gramToString (TokenGram token)  =   token
gramToString _                  =   ""

-- | Create a list of observed bigrams for a list of Tokens.
bigrams :: [Token] -> [BiGram]
bigrams []           = []
bigrams tokens@(x:_) = (StartGram, TokenGram x) : bigrams_ tokens
    where
        bigrams_ (x:[])     = [(TokenGram x, EndGram)]
        bigrams_ (x:y:rest) = (TokenGram x, TokenGram y) : bigrams_ (y:rest)

-- | Given a list of observed BiGrams, calculate a language model.
createModel ::  (MonadRandom r) => [BiGram] -> BiGramModel r
createModel =   (M.map rvarFromObservations)
            .   M.fromListWith (++)
            .   map (second return)
    where
        rvarFromObservations :: (MonadRandom r) => [Gram] -> r Gram
        rvarFromObservations events = sample dist
            where dist = fromObservations events :: Categorical Double Gram

-- | Randomly generates a string based on a given BiGramModel.
utterance :: (MonadRandom r) => BiGramModel r -> r String
utterance model = liftM (cleanUp . unwords . map gramToString) $ generate StartGram
    where 
        generate EndGram = return []
        generate current = do
            newGram <-  fromMaybe (return EndGram) $ M.lookup current model
            rest    <-  generate newGram
            return (newGram : rest)

        -- Hand curated rules for fixing spacing after tokenizing
        cleanUp []                          =   []
        cleanUp (' ':',':[])                =   ","
        cleanUp (' ':'.':[])                =   "."
        cleanUp (' ':'?':[])                =   "?"
        cleanUp (' ':'!':[])                =   "!"
        cleanUp (' ':'.':'.':xs)            =   ".." ++ cleanUp xs
        cleanUp (' ':'!':'!':xs)            =   "!!" ++ cleanUp xs
        cleanUp (' ':',':' ':xs)            =   ", " ++ cleanUp xs
        cleanUp (' ':'.':' ':xs)            =   ". " ++ cleanUp xs
        cleanUp (' ':'?':' ':xs)            =   "? " ++ cleanUp xs
        cleanUp (' ':'!':' ':xs)            =   "! " ++ cleanUp xs
        cleanUp (' ':'\'':'d':' ':xs)       =   "'d " ++ cleanUp xs
        cleanUp (' ':'\'':'m':' ':xs)       =   "'m " ++ cleanUp xs
        cleanUp (' ':'\'':'s':' ':xs)       =   "'s " ++ cleanUp xs
        cleanUp (' ':'\'':'n':'t':' ':xs)   =   "'nt " ++ cleanUp xs
        cleanUp (' ':'\'':'v':'e':' ':xs)   =   "'ve " ++ cleanUp xs
        cleanUp (x:xs)                      =   x : cleanUp xs
