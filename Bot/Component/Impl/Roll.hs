module Bot.Component.Impl.Roll (
    rollDice
)   where

import Bot.Component
import Bot.Component.Command
import Bot.IO

import Control.Monad.Trans
import Data.Char
import System.Random

-- | Implements the "!roll sides" command, where the bot replies with a roll of
-- a random sides sided dice.
rollDice :: Bot BotComponent
rollDice = command "!roll" rollAction
    where
        rollAction = (ircReply =<<) . randomI . takeWhile isDigit . unwords

randomI :: String -> Bot String
randomI []  =   return "Pigup Pigup Pigup"
randomI i   =   liftIO (fmap (randomR (1, (read i :: Int))) newStdGen) 
            >>= (\x -> return $ show $ fst x) 

