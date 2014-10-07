module Bot.Component.Impl.Roll (
    rollDice
)   where

import Bot.Component
import Bot.Component.Command
import Bot.IO

import Control.Monad
import Data.Char
import System.Random

-- | Implements the "!roll sides" command, where the bot replies with a roll of
-- a random sides sided dice.
rollDice :: Bot Component
rollDice = (command "!roll" rollAction) `withHelpMessage` help
    where
        help = helpForCommand "roll" ["usage: !roll number"]

        rollAction = (ircReplyMaybe =<<) . randomI . takeWhile isDigit . unwords

randomI :: String -> Bot (Maybe String)
randomI []  = return Nothing
randomI i   | n > 0     = liftM (Just . show . fst)
                        $ liftIO (fmap (randomR (1, n)) newStdGen)
            | otherwise = return Nothing
    where
        n = read i :: Integer

