module Bot.Component.Impl.Github (
    github
)   where

import Bot
import Bot.Component
import Bot.Component.Impl.Reboot
import Bot.Component.Regex

import Control.Monad.Trans.Identity

github :: Bot Component
github = mkComponentT $ regexT pattern action
    where
        -- Matches the message the github nick sends to the channel whenever
        -- there is a new commit in the zhenya_bot repo.
        pattern = "^...\\[...zhenya_bot....\\] ...[^ ]+. pushed .[0-9]+. new commit(s?) to ...master.:"

        action :: String -> IdentityT Bot ()
        action  =   const $ (setExitCode Update
                >>  (liftBot $ liftIO $ putStrLn "hey......"))
