module Bot.Time (
    pretty
)   where

import Control.Arrow
import Data.List
import System.Time

-- | Pretty print a TimeDiff.
pretty :: TimeDiff -> String
pretty td   =   unwords 
            $   map (uncurry (++) . first show) 
            $   if null diffs 
                    then [(0, "s")]
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

