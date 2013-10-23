module Bot.Component.Impl.Reboot (
    BotExitCode (..)
,   reboot
,   setExitCode
)   where

import Bot.Component
import Bot.Component.Command
import Bot.Component.Combinator

import Control.Monad.State
import Control.Monad.Trans.Identity
import System.Exit

data BotExitCode = Quit | Restart | Update

-- | Expose this functionality for other components to use.
setExitCode :: BotMonad b => BotExitCode -> b ()
setExitCode Quit    = setExitCode' ExitSuccess
setExitCode Restart = setExitCode' $ ExitFailure 100
setExitCode Update  = setExitCode' $ ExitFailure 101

-- | Help for setExitCode that takes an `ExitCode`
setExitCode' exitCode = liftBot $ modify (\s -> s {exitCode = Just exitCode})

-- | Handle the !quit, !restart, and !update commands.
reboot :: Bot Component
reboot = mkComponentT $ quit +++ restart +++ update

quit :: String -> IdentityT Bot ()
quit = simpleCommandT "!quit" (setExitCode Quit)

restart :: String -> IdentityT Bot ()
restart = simpleCommandT "!restart" (setExitCode Restart)

update :: String -> IdentityT Bot ()
update = simpleCommandT "!update" (setExitCode Update)
