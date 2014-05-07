module Bot.Component.Impl.Stalker (
    StalkerHandle
,   NickStatus (..)
,   newStalkerHandle
,   stalker
,   getNicksOnChannel
)   where

import Bot.Component
import Bot.IO

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M

-- | Denotes if the nick has op, voice, or nothing in a channel.
data NickStatus = Op | Voice | Normal
    deriving (Eq, Ord, Show, Read)

-- | The internal state of the stalker service. The datatype is opaque and
-- should only be accessed through the exposed API.
data StalkerInfo = StalkerInfo {
    -- | A mapping from channels to nicks.
    nicks     :: M.Map String [(NickStatus, String)]
    -- | Temporary mapping for storing in progress NAMES replies.
,   tmpNicks  :: M.Map String [(NickStatus, String)]
}

-- | Opaque type for the StalkerInfo that is exposed externally.
type StalkerHandle = TVar StalkerInfo

-- | Creates a new StalkerHandle for use with the stalker component.
-- A reference to this required in order to make API calls.
newStalkerHandle :: IO StalkerHandle
newStalkerHandle =
    liftIO $ newTVarIO StalkerInfo {nicks = M.empty, tmpNicks = M.empty}

-- | The stalker component keeps an eye out for people JOINing, PARTing, and
-- QUITting the channel. It also notes NICK changes.
stalker :: StalkerHandle -> Bot Component
stalker handle = mkComponent $ \message ->
    case words message of
        -- Conditions upon which to request a NAMES update.
        _:"QUIT":_  ->  ircNames
        _:"PART":_  ->  ircNames
        _:"NICK":_  ->  ircNames
        _:"JOIN":_  ->  ircNames
        _:"MODE":_  ->  ircNames

        -- Handling of NAMES updates.
        _:"353":_:_:channel:nicks ->  addNicksToChannel handle channel nicks
        _:"366":_:channel:_       ->  finishChannel handle channel

        _           ->  return ()

-- | Send a NAMES query to the current channel.
ircNames :: Bot ()
ircNames    =   gets currentChannel
            >>= ircWrite "NAMES "

addNicksToChannel :: StalkerHandle -> String -> [String] -> Bot ()
addNicksToChannel handle channel nicks =
    liftIO $ atomically $ modifyTVar' handle (updateTmpNicks channel nicks)

updateTmpNicks :: String -> [String] -> StalkerInfo -> StalkerInfo
updateTmpNicks channel names info = info {
    tmpNicks = M.insertWith (++) channel (map grabStatus names) (tmpNicks info)
}

grabStatus :: String -> (NickStatus, String)
grabStatus ('@':nick) = (Op, nick)
grabStatus ('+':nick) = (Voice, nick)
grabStatus (':':nick) = grabStatus nick
grabStatus nick       = (Normal, nick)

finishChannel :: StalkerHandle -> String -> Bot ()
finishChannel handle channel =
    liftIO $ atomically $ modifyTVar' handle (swapTmpChannel channel)

swapTmpChannel :: String -> StalkerInfo -> StalkerInfo
swapTmpChannel channel info = info {
    nicks     = M.insert
                  channel
                  (concat $ maybeToList $ channel `M.lookup` (tmpNicks info))
                  (nicks info)
,   tmpNicks  = M.insert channel [] (tmpNicks info)
}

getNicksOnChannel :: StalkerHandle -> String -> Bot [(NickStatus, String)]
getNicksOnChannel handle channel =
        concat . maybeToList . (channel `M.lookup`) . nicks
    <$> (liftIO $ readTVarIO handle)
