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
queen a 
      | n > 14 = return $ Just "I better not shoot myself by answering that!"
      | otherwise = liftM ( Just . show )
         $ return $ head [x | x <- permutations [1..n], safe x]
   where
      n = read a :: Int
  
safe :: [Int] -> Bool
safe [] = True
safe (x:xs) = and (map (\f -> safeDiag (f) (f x 1) xs) [ (+), (-) ]) && safe
  xs
   
safeDiag :: ( Int -> Int -> Int ) -> Int -> [Int] -> Bool
safeDiag _ _ [] = True
safeDiag f field (x:xs) = field /= x && safeDiag f y xs
  where y = f x 1
