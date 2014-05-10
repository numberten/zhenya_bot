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

-- | The lists component allows users to keep lists of things.
-- It can be queried with the !list command.
lists :: Bot Component
lists = persistent "lists.txt" (commandT usage "!list" listsAction) initialState
    where
        initialState :: Bot (M.Map String [(String,Bool)])
        initialState = return M.empty

        listsAction ("show":[])             = showLists
        listsAction ("show":list:[])        = showList list
        listsAction ("add":list:[])         = addList list
        listsAction ("add":"at":i:list:xs)  = addElemAt i list xs
        listsAction ("add":list:xs)         = addElem list xs
        listsAction ("rm":list:[])          = rmList list
        listsAction ("rm":list:xs)          = rmElem list xs
        listsAction ("check":list:e:xs)     = checkOffElem list (e:xs)
        listsAction ("flush":list:[])       = flushList list
        listsAction _                       = return ()

        -- The usage message, in case no arguments are passed.
        usage = UsageMessage [  
                  "!list show [list]"
              ,   "!list add [at index] list [element]"
              ,   "!list rm list [element]"
              ,   "!list check list element"
              ,   "!list flush list"
              ]

        -- Displays the list of lists.
        showLists = do
            listMap         <-  get
            let speakKeys   =   fmap ircReply 
                            $   M.keys listMap
            case speakKeys of
                []  ->  liftBot 
                    $   ircReply "Such missing! Much not lists! Wow."
                _   ->  liftBot $ sequence_ speakKeys

        -- Displays the elements in a given list l.
        -- If l does not exist, exits cleanly.
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
                                    .   map (\(i,(n,b)) 
                                        -> (show i ++ ") "
                                        ++ if b then st n else n)) 
                                    $   zip [1..] xs
            where
                st  =   (("\204\182" ++) . return =<<)

        -- Makes a new empty list l.
        -- Adds it to the list of lists.
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

        -- Appends an existing list l with a new string xs.
        -- If l does not exist, exits cleanly.
        -- If xs aleady exists within l, exits cleanly.
        addElem l xs = do
            listMap     <-  get
            let result  =   M.lookup l listMap
            let x       =   unwords xs
            case result of 
                Nothing ->  liftBot 
                        .   ircReply 
                        $   "There is no list '"++l++"'."
                Just ls ->  do
                    let result2 = lookup x ls
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

        -- Inserts a new string xs into list l at index i.
        -- If l does not exist, exits cleanly.
        -- If xs already exists within l, exits cleanly.
        -- If i can not be parsed as an number, exits cleanly.
        addElemAt i l xs = do
            listMap     <-  get
            let result  =   M.lookup l listMap
            let x       =   unwords xs
            case result of
                Nothing ->  liftBot
                        .   ircReply
                        $   "There is no list '"++l++"'."
                Just ls ->  do
                    let b   =   all isNumber i
                    case b of
                        False   ->  liftBot
                                .   ircReply
                                $   "There is no index '"
                                ++  i
                                ++  "' into list '"
                                ++  l
                                ++  "'."
                        True    -> do 
                            let result2 = lookup x ls
                            case result2 of
                                Nothing -> do
                                    let i'  =   (read i - 1)
                                    let f   =   \y z 
                                            ->  y++[(x,False)]++z
                                    put     $   M.update (\lx -> return
                                                $ uncurry f 
                                                (splitAt i' lx)) 
                                                l 
                                                listMap
                                    liftBot .   ircReply
                                            $   "'" 
                                            ++  x 
                                            ++  "' added to '"
                                            ++  l 
                                            ++  "' at index '"
                                            ++  i
                                            ++  "'."
                                Just _  ->  liftBot
                                        .   ircReply
                                        $   "'"
                                        ++  x
                                        ++  "' already exists within '"
                                        ++  l
                                        ++  "'."

        -- Removes a list l from the list of lists.
        -- If l does not exist, exits cleanly.
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

        -- Removes an element xs from an existing list l.
        -- If either l or xs don't exist, exits cleanly.
        -- xs can be either the string stored, or its index.
        rmElem l xs = do 
            listMap     <-  get
            let result  =   M.lookup l listMap
            let x       =   unwords xs
            case result of 
                Nothing ->  liftBot
                        .   ircReply
                        $   "There is no list '"++l++"'."
                Just ls ->  do
                    --Safe due to short circuit.
                    let result1 =   lookup x ls
                    let result2 =   (,) result1
                                $   all isNumber x
                                &&  read x
                                <=  length ls
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
                -- Helper function for removing element, regardless of 
                -- whether or not 'rmElem' is passed the string to be
                -- deleted or its current index.
                remove  :: String
                        -> String
                        -> M.Map String [(String,Bool)]
                        -> M.Map String [(String,Bool)]
                remove x key dic    |   and $ map isNumber x
                                    =   let i = read x
                                        in M.update (\lx -> return 
                                            $ take (i-1) lx ++ (drop i lx))
                                              key 
                                              dic
                                    |   otherwise
                                    =   M.update (return 
                                        .  filter (\(n,_) 
                                        ->  n /= x)) 
                                            key
                                            dic

        -- Checks an element l off from an existing list l.
        -- If either l or xs don't exist, exits cleanly.
        -- xs can be either the string stored, or its index.
        -- Will un-checkoff an already checked off element.
        checkOffElem l xs = do 
            listMap     <-  get
            let result  =   if xs == [] 
                                then Nothing 
                                else M.lookup l listMap
            let x       =   unwords xs
            case result of 
                Nothing ->  liftBot
                        .   ircReply
                        $   "There is no list '"++l++"'."
                Just ls ->  do
                    --Safe due to short circuit.
                    let result1 =   lookup x ls
                    let result2 =   (,) result1
                                $   all isNumber x
                                &&  read x
                                <=  length ls
                    case result2 of
                        (Nothing,False) ->  liftBot
                                        .   ircReply 
                                        $   "'"
                                        ++  x
                                        ++  "' does not exist within list '"
                                        ++  l
                                        ++  "'."
                        _           ->  do 
                            put     $   check x l listMap 
                            liftBot .   ircReply  
                                    $   "'"++x++"' checked off from list '"++l++ "'."
            where
                -- Flips the checked flag.
                flip' (str,b) = (str,not b)
                -- Helper function for flipping checked bit.
                -- Accepts both the name of elements to check off,
                -- or their indexes in a given list.
                check   :: String
                        -> String
                        -> M.Map String [(String,Bool)]
                        -> M.Map String [(String,Bool)]
                check x key dic |   and $ map isNumber x
                                = do
                                    let i       =   read x
                                    let start   =   take (i - 1)
                                    let end'    =   drop (i-1)
                                    let mid     =   flip'
                                                .   head 
                                                .   end'
                                    let end     =   tail
                                                .   end'
                                    let f       =   \ls 
                                                ->  return
                                                $   (start ls)
                                                ++  [mid ls]
                                                ++  (end ls)
                                    M.update f key dic
                                |   otherwise
                                = do 
                                    let f'  =   \t@(n,_)
                                            ->  if n == x
                                                    then flip' t
                                                    else t
                                    let f   = return . map f'
                                    M.update f key dic

        -- Flushes some list, l. Flushing removes
        -- all checked off elements. Exits cleanly,
        -- if no list l exists.
        flushList l = do
            listMap     <-  get
            let result  =   M.lookup l listMap
            case result of
                Nothing ->  liftBot
                        .   ircReply
                        $   "There is no list '"++l++"'."
                Just _  ->  do
                    let f   =   filter (\(_,b) -> not b)
                    put     $   M.map f listMap
                    liftBot .   ircReply
                            $   "Flushing '"++l++"'."
