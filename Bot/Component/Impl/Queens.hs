module Bot.Component.Impl.Queens (
    calcQueens
)   where

import Bot.Component
import Bot.Component.Command
import Bot.IO

import Control.Monad
import Data.Char
import Data.List

calcQueens :: Bot Component
calcQueens = command "!queens" setQueens
    where
        setQueens = (ircReplyMaybe =<<) . queen . takeWhile isDigit . unwords

queen :: String -> Bot ( Maybe String )
queen a = liftM ( Just . show ) $ return $ head $ foldM foldingFunction [] [1..n]
   where
      -- Our folding function. It works like this:
      -- list: a list of already known safe positions from the previous fold
      -- _: our input. But we don't need it as the input is only there to restrict the number of elements in the result lists
      -- The function itself returns a list of lists, with the inner lists each having a position prepended that is safe
      foldingFunction list _ = [x:list | x <- [1..n] \\ list, safe (x:list)]
      n = read a :: Int

safe :: [Int] -> Bool
safe [] = True
safe (x:xs) = and (map (\f -> safeDiag (f) (f x 1) xs) [ (+), (-) ])

safeDiag :: ( Int -> Int -> Int ) -> Int -> [Int] -> Bool
safeDiag _ _ [] = True
safeDiag f field (x:xs) = field /= x && safeDiag f y xs
  where y = f x 1
