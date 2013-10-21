module Bot.Component.Impl.Reboot (
    reboot
)   where

import Bot.Component
import Bot.Component.Command
import Bot.Component.Combinator

import Control.Monad.State
import Control.Monad.Trans.Identity
import System.Exit

-- | Handle the !quit, !restart, and !update commands.
reboot :: Bot Component
reboot = mkComponentT $ quit +++ restart +++ update

setExitCode :: ExitCode -> IdentityT Bot ()
setExitCode exitCode = lift $ modify (\s -> s {exitCode = Just exitCode})

quit :: String -> IdentityT Bot ()
quit = simpleCommandT "!quit" (setExitCode ExitSuccess)

restart :: String -> IdentityT Bot ()
restart = simpleCommandT "!restart" (setExitCode $ ExitFailure 100)

update :: String -> IdentityT Bot ()
update = simpleCommandT "!update" (setExitCode $ ExitFailure 101)
