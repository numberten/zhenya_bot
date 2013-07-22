{-# LANGUAGE ScopedTypeVariables #-}
module Bot.Component.Impl.FileSearch (
    fileSearch
)   where

import Bot.Component
import Bot.Component.Combinator
import Bot.Component.Command
import Bot.Component.Stateful
import Bot.IO

import              Control.Monad.State
import              Control.Monad.Catch
import              Data.Char
import              Data.List
import qualified    Data.Map as M
import              Data.Monoid
import qualified    Data.Set as S
import              Data.Time
import              Prelude hiding (catch)
import              System.FilePath
import              System.IO hiding (hGetContents)
import              System.IO.Strict (hGetContents)

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
        createIndex handle  =   hGetContents handle
                            >>= return
                            .   foldr parseFilePath M.empty
                            .   lines

        -- Adds a single file path to an index
        parseFilePath filepath index =
            let terms = words $ map toLower $ takeBaseName filepath
            in  foldr (\term -> M.insertWith (<>) term [filepath]) index terms

        -- Every 10 minutes reload the index
        reloadCatalogue _ = do
            (lastLoad,_)    <-  get
            now             <-  liftIO getCurrentTime
            let timePassed  =   diffUTCTime now lastLoad
            let tenMinutes  =   10*60*60
            when (timePassed > tenMinutes) $
                lift initialState >>= put

        fileSearchCommand = commandT "!files" fileSearchAction

        -- Given a list of search terms, responds with a list of files sorted by
        -- relevance
        fileSearchAction terms = do
            (_,index)       <-  get
            let keys        =   M.keys index
            let matches     =   sortBy compareFileNameLength
                            $   S.elems
                            $   foldl1 S.intersection
                            $   map S.unions
                            $   map (map S.fromList)
                            $   map (map (index M.!))
                            $   map (\t -> filter (t `isInfixOf`) keys)
                            $   map (map toLower) terms
            case matches of
                []      ->  lift 
                        $   ircReply "No matches found. Try using fewer terms?"
                matches ->  do
                    lift $ mapM_ ircReply $ take 5 matches
                    when (length matches > 5) $
                        lift $ ircReply "Limiting to top 5 results..."

        compareFileNameLength a b = compare lengthA lengthB
            where
                fileNameA   = takeBaseName a ++ takeExtension a
                fileNameB   = takeBaseName b ++ takeExtension b
                lengthA     = length fileNameA
                lengthB     = length fileNameB
