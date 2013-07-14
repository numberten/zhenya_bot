{-# LANGUAGE RecordWildCards #-}
module Bot.Component.Impl.Seen (
    seen
)   where

import              Bot.Component
import              Bot.Component.Command
import              Bot.Component.Conditional
import              Bot.Component.Combinator
import              Bot.Component.Stateful
import              Bot.IO
import              Bot.Time

import              Control.Applicative
import              Control.Monad.State
import qualified    Data.Map as M
import              Debug.Trace
import              System.Time

-- | The standard ClockTime definition does not define a Read Instance?? So to
-- we redefine both the Show and Read in a newtype.
newtype SeenTime = SeenTime ClockTime
    deriving (Eq)

instance Show SeenTime where
    show (SeenTime (TOD a b)) = show (a,b)

instance Read SeenTime where
    readsPrec precedence string = do
        ((a,b), leftOver)   <-  readsPrec precedence string
        return (SeenTime $ TOD a b, leftOver)

type TimeMap = M.Map String SeenTime

seen :: Bot BotComponent
seen = persistent "seen.txt" action initialState
    where
        action          = seenLogger +++ seenCommand
        initialState    = return M.empty

seenLogger :: String -> StateT TimeMap Bot ()
seenLogger = conditionalT (\_ -> True) seenLoggerAction

seenLoggerAction :: StateT TimeMap Bot ()
seenLoggerAction = do
    BotState{..}    <-  lift get
    now             <-  liftIO getClockTime
    modify (M.insert currentNick $ SeenTime now)

seenCommand :: String -> StateT TimeMap Bot ()
seenCommand = commandT "!seen" seenCommandAction

seenCommandAction :: [String] -> StateT TimeMap Bot ()
seenCommandAction (nick:_)  = do
    lastSeen <- M.lookup nick <$> get
    case lastSeen of
        Just (SeenTime lastSeen)    -> do
            now         <-  liftIO getClockTime
            let diff    =   pretty $ diffClockTimes now lastSeen
            lift $ ircReply $ "I last saw them speak " ++ diff ++ " ago."
        Nothing                     ->  
            lift $ ircReply "I have not seen them speak."
seenCommandAction _         =   return ()

