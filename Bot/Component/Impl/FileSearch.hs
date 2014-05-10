{-# LANGUAGE ScopedTypeVariables #-}
module Bot.Component.Impl.FileSearch (
    fileSearch
)   where

import Bot.Component
import Bot.Component.Combinator
import Bot.Component.Command
import Bot.Component.Stateful
import Bot.IO

import Control.Monad
import Control.Monad.State
import Control.Monad.Catch
import Data.Char
import Data.List
import Data.Monoid
import Data.Time
import Prelude hiding (catch)
import System.FilePath
import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)
import qualified Data.Map as M
import qualified Data.Set as S

fileSearch = stateful (fileSearchCommand +++ reloadCatalogue) initialState
    where
        -- The initial state is the time the current index was loaded, and the
        -- index of files
        initialState = do
            index   <-  loadCatalogue
            now     <-  liftIO getCurrentTime
            return (now, index)

        -- Where should we look for the catalogue?
        cataloguePath = do
            directory <- gets dataDirectory
            return (directory </> "filesearch.txt")

        -- Loads a file catalogue from disk
        loadCatalogue = do
            path <- cataloguePath
            liftIO (withFile path ReadMode createIndex)
                `catch` (\(_ :: SomeException) -> return M.empty)

        -- Given the contents of a catalogue, create a mapping from terms to
        -- files
        createIndex handle  =   liftM (foldr parseFilePath M.empty . lines)
                            $   hGetContents handle

        -- Adds a single file path to an index
        parseFilePath filepath index =
            let terms   =   words
                        $   map toLower
                        $   concat [
                                takeBaseName filepath
                            ,   " "
                            ,   takeExtension filepath
                            ]
            in  foldr (\term -> M.insertWith (<>) term [filepath]) index terms

        -- Every 10 minutes reload the index
        reloadCatalogue _ = do
            (lastLoad,_)    <-  get
            now             <-  liftBot $ liftIO getCurrentTime
            let timePassed  =   diffUTCTime now lastLoad
            let tenMinutes  =   10*60*60
            when (timePassed > tenMinutes) $
                liftBot initialState >>= put

        fileSearchCommand = commandT usage "!files" fileSearchAction

        -- The usage message, in case no arguments are passed.
        usage = UsageMessage ["usage: !files string"]

        -- Given a list of search terms, responds with a list of files sorted by
        -- relevance
        fileSearchAction terms = do
            (_,index)       <-  get
            let keys        =   M.keys index
            let termToFiles =   S.unions
                            .   map (S.fromList . (index M.!))
                            .   (\t -> filter (t `isInfixOf`) keys)
                            .   map toLower
            let matches     =   sortBy compareFileNameLength
                            $   S.elems
                            $   foldl1 S.intersection
                            $   map termToFiles terms
            case matches of
                []      ->  liftBot
                        $   ircReply "No matches found. Try using fewer terms?"
                matches ->  do
                    liftBot $ mapM_ ircReply $ take 5 matches
                    when (length matches > 5) $
                        liftBot $ ircReply "Limiting to top 5 results..."

        compareFileNameLength a b = compare lengthA lengthB
            where
                fileNameA   = takeBaseName a ++ takeExtension a
                fileNameB   = takeBaseName b ++ takeExtension b
                lengthA     = length fileNameA
                lengthB     = length fileNameB
