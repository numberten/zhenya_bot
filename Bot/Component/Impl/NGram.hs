{-#LANGUAGE TupleSections#-}
module Bot.Component.Impl.NGram (
    imitate
)   where

import Bot.Component
import Bot.Component.Command
import Bot.Component.Impl.NickCluster
import Bot.Component.Stateful
import Bot.IO

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Trans.Identity
import Data.Char
import Data.List
import Data.Maybe
import Data.Random
import Data.Random.Distribution.Categorical
import NLP.Tokenize
import System.FilePath
import qualified Data.Map as M

type Name = String
type Token = String
type BiGram = (Gram, Gram)
type BiGramModel = M.Map Gram (Integer, Categorical Double Gram)

data Gram = StartGram | EndGram | TokenGram Token
    deriving (Show, Eq, Ord, Read)

type ImitateState = M.Map String BiGramModel

-- | Speak like the various members of the IRC channel.
imitate :: ClusterNickHandle -> Bot Component
imitate handle = stateful commandAction initialState
    where
        -- Where should we look for the catalogue?
        logPath = do
            directory <- gets dataDirectory
            return (directory </> "ngram.txt")

        -- Loads a preprocessed version of the log and creates a BiGramModel for
        -- each unique nick seen.
        initialState :: Bot ImitateState
        initialState = do
            log           <-  (logPath >>= liftIO . readFile)
                                  `catchError` const (return "")
            let logLines  =   lines log
            let messages  =   groupByName $ map processLine logLines
            let state     =   M.map (createModel . concatMap bigrams) messages
            -- Force the evaluation of each model
            liftIO $ mapM_ (utterance . snd) $ M.toList state
            return state

        -- Returns the nick of the same cluster as the one given that is used as
        -- the key for the map of nicks to models.
        canonicalNick   ::  String
                        ->  StateT ImitateState (IdentityT Bot) (Maybe String)
        canonicalNick nick = do
            modelMap    <-  get
            nicks       <-  liftBot $ aliasesForNick handle nick
            return $ listToMaybe $ filter (`elem` nicks) $ M.keys modelMap

        -- Merge all known bigram models based on known alias clusters.
        mergeModels :: StateT ImitateState (IdentityT Bot) ()
        mergeModels = do
            modelMap        <-  get
            clusters        <-  liftBot $ allNickAliases handle
            let newModelMap =   foldr collectNickKeys modelMap clusters
            put newModelMap

        -- Used by mergeModels, given a cluster (list) of nicks and a mapping
        -- from nicks to BiGramModels, merge all of the nicks from the given
        -- cluster that are also keys in the map.
        collectNickKeys :: [String] -> ImitateState -> ImitateState
        collectNickKeys [] modelMap                 = modelMap
        collectNickKeys nicks@(cnick:_) modelMap    = resultingMap
            where
                modelMapSansKey =   foldr M.delete modelMap nicks

                mergedValue     =   fmap (foldr1 mergeModel)
                                $   (>>) <$> listToMaybe <*> return
                                $   mapMaybe (`M.lookup` modelMap) nicks

                resultingMap    =   foldr (M.insert cnick) modelMapSansKey
                                $   maybeToList mergedValue

        -- First attempts to group nicks together based on clusters, and then
        -- generates a sentence in the literary stylings of the given nick.
        commandAction = commandT usage "!be" $ \args -> case args of
            [nick]  -> do
                mergeModels
                keyNick         <-  canonicalNick nick
                let sayMessage  =   keyNick >>= \keyNick -> return $ do
                    model       <-  gets (M.! keyNick)
                    message     <-  liftBot $ liftIO $ utterance model
                    liftBot $ ircReply message
                fromMaybe (liftBot $ ircReply "not a guy.") sayMessage
            _       -> liftBot $ ircReply "be who?"

        -- The usage message, in case no arguments are passed.
        usage = UsageMessage ["usage: !be nick"]

-- | Process a line from the specially formatted log file.
processLine :: String -> (Name, [Token])
processLine line = (name, tokenize $ map toLower message)
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
        bigrams_ ([])       = []
        bigrams_ (x:[])     = [(TokenGram x, EndGram)]
        bigrams_ (x:y:rest) = (TokenGram x, TokenGram y) : bigrams_ (y:rest)

-- | Given a list of observed BiGrams, calculate a language model.
createModel ::  [BiGram] -> BiGramModel
createModel =   M.map ((,) <$> fromIntegral . length <*> fromObservations)
            .   M.fromListWith (++)
            .   map (second return)

-- | Merges two BiGramModels, properly weighting the grams by frequency counts.
mergeModel :: BiGramModel -> BiGramModel -> BiGramModel
mergeModel = M.unionWith mergeDist
    where
        mergeDist (numA, distA) (numB, distB) = (numNew, distNew)
            where
                numNew  = numA + numB
                eventsA = adjustedList numA distA
                eventsB = adjustedList numB distB
                distNew = collectEvents $ fromList $ eventsA ++ eventsB

                adjustedList num = map (first weight) . toList
                    where weight = (* (fromIntegral num / fromIntegral numNew))

-- | Randomly generates a string based on a given BiGramModel.
utterance :: (MonadRandom r) => BiGramModel -> r String
utterance model =   liftM (cleanUp . unwords . map gramToString)
                $   generate StartGram
    where
        generate EndGram = return []
        generate current = do
            newGram <-  fromMaybe (return EndGram)
                    $   (sample . snd)
                    <$> M.lookup current model
            rest    <-  generate newGram
            return (newGram : rest)

        -- Hand curated rules for fixing spacing after tokenizing
        cleanUp []                          =   []
        cleanUp (' ':';':xs)                =   ";" ++ cleanUp xs
        cleanUp (' ':':':xs)                =   ":" ++ cleanUp xs
        cleanUp (' ':')':xs)                =   ")" ++ cleanUp xs
        cleanUp ('(':' ':xs)                =   "(" ++ cleanUp xs
        cleanUp (' ':',':xs)                =   "," ++ cleanUp xs
        cleanUp (' ':'.':xs)                =   "." ++ cleanUp xs
        cleanUp (' ':'?':xs)                =   "?" ++ cleanUp xs
        cleanUp (' ':'!':xs)                =   "!" ++ cleanUp xs
        cleanUp (' ':'\'':'d':' ':xs)       =   "'d " ++ cleanUp xs
        cleanUp (' ':'\'':'m':' ':xs)       =   "'m " ++ cleanUp xs
        cleanUp (' ':'\'':'s':' ':xs)       =   "'s " ++ cleanUp xs
        cleanUp (' ':'\'':'n':'t':' ':xs)   =   "'nt " ++ cleanUp xs
        cleanUp (' ':'n':'\'':'t':' ':xs)   =   "n't " ++ cleanUp xs
        cleanUp (' ':'\'':'v':'e':' ':xs)   =   "'ve " ++ cleanUp xs
        cleanUp (x:xs)                      =   x : cleanUp xs
