module Bot.Component.Impl.Lists (
    lists
)   where

import Bot.Component
import Bot.Component.Command
import Bot.Component.Stateful
import Bot.IO

import Control.Monad.State
import Data.Char (isNumber)
import qualified Data.Map as M

lists :: Bot Component
lists = persistent "lists.txt" (commandT "!list" listsAction) initialState
    where
        initialState :: Bot (M.Map String [(String,Bool)])
        initialState = return M.empty

        listsAction ("show":[])         = showLists
        listsAction ("show":list:[])    = showList list
        listsAction ("add":list:[])     = addList list
        listsAction ("add":list:xs)     = addElem list xs
        listsAction ("rm":list:[])      = rmList list
        listsAction ("rm":list:xs)      = rmElem list xs
        listsAction _                   = printUsageMessage

        printUsageMessage = liftBot $ do
            ircReply "!list show [list]"
            ircReply "!list add list [element]"
            ircReply "!list rm list [element]"

        showLists = do
            listMap         <-  get
            let speakKeys   =   fmap ircReply 
                            $   M.keys listMap
            case speakKeys of
                [] -> liftBot $ ircReply "Such missing! Much not lists! Wow."
                _  -> liftBot $ sequence_ speakKeys

        showList l = do
            listMap     <-  get
            let result  =   M.lookup l listMap
            case result of
                Nothing ->  liftBot 
                        .   ircReply 
                        $   "There is no list '"++l++"'."
                Just xs ->  case xs of
                                []  ->  liftBot
                                    $   ircReply "Much empty. Such void. Wow!"
                                _   ->  liftBot 
                                    $   sequence_
                                    .   map ircReply
                                    .   map (\(i,(n,_)) -> (show i ++ ") "++n)) 
                                    $   zip [1..] xs

        addList l = do
            listMap     <-  get
            let result  =   M.member l listMap
            case result of
                True    ->  liftBot 
                        .   ircReply 
                        $   "There is already a list '"++l++"'."
                False   ->  do
                    put     $   M.insert l [] listMap
                    liftBot .   ircReply 
                            $   "'"++l++"' added to the list of lists."

        addElem l xs = do
            listMap     <-  get
            let result  =   M.lookup l listMap
            let x       =   unwords xs
            case result of 
                Nothing ->  liftBot 
                        .   ircReply 
                        $   "There is no list '"++l++"'."
                Just xs ->  do
                    let result2 = lookup x xs
                    case result2 of
                        Nothing -> do
                            put     $   M.update (\lx -> return
                                            $ lx++[(x,False)]) l listMap
                            liftBot .   ircReply
                                    $   "'" 
                                    ++  x 
                                    ++  "' added to '"
                                    ++  l 
                                    ++  "'."
                        Just _  ->  liftBot
                                .   ircReply
                                $   "'"++x++"' already exists within '"++l++"'."

        rmList l = do
            listMap     <-  get
            let result  =   M.member l listMap
            case result of
                True    ->  liftBot
                        .   ircReply
                        $   "Deleting list '"++l++"'."
                False   ->  liftBot
                        .   ircReply 
                        $   "There is no list '"++l++"'."
            put         $   M.delete l listMap

        rmElem l xs = do 
            listMap     <-  get
            let result  =   M.lookup l listMap
            let x       =   unwords xs
            case result of 
                Nothing ->  liftBot
                        .   ircReply
                        $   "There is no list '"++l++"'."
                Just xs ->  do
                    --Safe due to short circuit.
                    let result1 =   lookup x xs
                    let result2 =   (,) result1
                                $   all isNumber x
                                &&  read x
                                <=  length xs
                    case result2 of
                        (Nothing,False) ->  liftBot
                                        .   ircReply 
                                        $   "'"
                                        ++  x
                                        ++  "' does not exist within list '"
                                        ++  l
                                        ++  "'."
                        _           ->  do 
                            put     $   remove x l listMap 
                            liftBot .   ircReply  
                                    $   "'"++x++"' removed from '"++l++ "'."
            where
                remove  :: String
                        -> String
                        -> M.Map String [(String,Bool)]
                        -> M.Map String [(String,Bool)]
                remove x key dic    |   and $ map isNumber x
                                    =   let i = read x
                                        in M.update (\lx -> return 
                                            $ take (i-1) lx ++ (drop i lx)) key dic
                                    |   otherwise
                                    =   M.update (return .  filter (\(n,_) -> n /= x)) key dic

--Too late for comments, will add tomorrow.
