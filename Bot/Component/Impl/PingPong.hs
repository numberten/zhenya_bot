module Bot.Component.Impl.PingPong (
    pingPong
)   where

import Bot.Component
import Bot.IO

-- | Respond to PING messages from the IRC server.
pingPong :: Bot Component
pingPong = mkComponent $ \message ->
    case words message of
        "PING":server:[]    ->  ircWrite "PONG" server
        _                   ->  return ()

