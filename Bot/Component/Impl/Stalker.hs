module Bot.Component.Impl.Stalker (
    stalker
)   where

import Bot.Component
import Bot.IO

-- | The stalker component keeps an eye out for people
-- JOINing, PARTing, and QUITting the channel. It also
-- notes NICK changes. 
stalker :: Bot Component
stalker = mkComponent $ \message ->
    case words message of
        _:"QUIT":_  ->  ircNames
        _:"PART":_  ->  ircNames
        _:"NICK":_  ->  ircNames
        _:"JOIN":_  ->  ircNames
        _:"MODE":_  ->  ircNames
        _           ->  return ()
