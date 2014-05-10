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
rollDice = command usage "!roll" rollAction
    where
        -- The usage message, in case no arguments are passed.
        usage = UsageMessage ["usage: !roll number"]
        rollAction = (ircReplyMaybe =<<) . randomI . takeWhile isDigit . unwords

randomI :: String -> Bot (Maybe String)
randomI []  =   return Nothing
randomI i   =   liftM (Just . show . fst)
            $   liftIO (fmap (randomR (1, read i :: Integer)) newStdGen)

