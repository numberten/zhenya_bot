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
calcQueens = command usage "!queens" $ sendEach . setQueens
    where
        -- The usage message, in case no arguments are passed.
        usage = UsageMessage ["usage: !queens number-of-queens"]

        sendEach = mapM_ ircReply
        setQueens = queen . takeWhile isDigit . unwords

queen :: String -> [String]
queen a
        | n > 25 = return "I better not shoot myself by answering that!"
        | otherwise = prettify $ getResult result
   where
      -- Our folding function. It works like this:
      -- list: a list of already known safe positions from the previous fold
      -- _: our input. But we don't need it as the input is only there to restrict the number of elements in the result lists
      -- The function itself returns a list of lists, with the inner lists each having a position prepended that is safe
      foldingFunction list _ = [x:list | x <- [1..n] \\ list, safe (x:list)]
      n = read a :: Int
      result = foldM foldingFunction [] [1..n]

prettify :: [Int] -> [String]
prettify [] = ["There is no solution for that."]
prettify list
        | length list > 12 = (show list):[]
        | otherwise = (show list):(map prettyRow list)
    where
        prettyRow x = (concat . replicate (x-1)) ". " ++ "Q " ++ (concat . replicate (n-x)) ". "
        n = length list

getResult :: [[Int]] -> [Int]
getResult [] = []
getResult (x:_) = x

safe :: [Int] -> Bool
safe [] = True
safe (x:xs) = and (map (\f -> safeDiag (f) (f x 1) xs) [ (+), (-) ])

safeDiag :: ( Int -> Int -> Int ) -> Int -> [Int] -> Bool
safeDiag _ _ [] = True
safeDiag f field (x:xs) = field /= x && safeDiag f y xs
  where y = f field 1
