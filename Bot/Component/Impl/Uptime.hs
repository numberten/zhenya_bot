module Bot.Component.Impl.Uptime (
    uptime
)   where

import Bot.Component
import Bot.Component.Command
import Bot.Component.Stateful
import Bot.IO

import Control.Arrow
import Control.Monad.State
import Data.List
import System.Time

-- | Report the IRC bot's uptime upon encountering the "!uptime" command.
uptime :: Bot BotComponent
uptime = stateful commandAction initialState
    where
        initialState = liftIO getClockTime

        commandAction = simpleCommandT "!uptime" $ do
            now     <- liftIO getClockTime
            zero    <- get
            lift $ ircReply $ pretty $ diffClockTimes now zero

-- | Pretty print a TimeDiff.
pretty :: TimeDiff -> String
pretty td   =   unwords 
            $   map (uncurry (++) . first show) 
            $   if null diffs 
                    then [(0, "s ")] 
                    else diffs
	where
		merge (tot,acc) (sec,typ) = 
		    let (sec', tot') = divMod tot sec 
		    in (tot', (sec', typ):acc)

		metrics = [(86400,"d"),(3600,"h"),(60,"m"),(1,"s")]

		diffs   = filter ((/=0) . fst) 
	            $ reverse 
	            $ snd 
	            $ foldl' merge (tdSec td, []) metrics

