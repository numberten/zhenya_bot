module Bot.Component.Fuzzy (
    fuzzyMatch
,   fuzzyMatchT
)   where

import Bot.Component
import Bot.Component.Conditional

import Control.Monad.Trans
import Control.Monad.Trans.Identity
import Data.Char
import Data.List.LCS.HuntSzymanski

-- | A fuzzy matching constructor that will execute an action when a message
-- roughly matches the target string. For more information about parameters see
-- `fuzzyMatchT`.
fuzzyMatch  :: String -> Float -> Bot () -> Bot Component
fuzzyMatch target threshold action =
    mkComponentT $ fuzzyMatchT target threshold actionT
    where
        actionT :: IdentityT Bot ()
        actionT = lift action

-- | A generic fuzzy matching constructor that can be used with monad
-- transformers like `Bot.Component.Stateful`.
fuzzyMatchT ::  BotMonad b
            -- | The target message that should trigger this component
            =>  String
            -- | How sensitive should the fuzzy matching algorithm be to
            -- differences? Smaller values are more sensitive. 0 is exact match,
            -- 1 will match anything.
            ->  Float
            -- | The action to be executed in the event of a match
            ->  b ()
            -- | Resulting Botable method
            ->  String -> b ()
fuzzyMatchT target threshold = conditionalT (match target threshold)


-- | Determines if two strings match based on their edit distance.
match ::
        -- | The target string
            String
        -- | The target threshold
        ->  Float
        -- | The string that we are testing against
        ->  String
        -- | Whether the strings are a match
        ->  Bool
match s f input   | length input > (4 * length s)  = False
                  | otherwise                      = fromIntegral (editDistance si $ lcs ni si)
                                                   <= (fromIntegral (length si) * f)
    where
        ni = map toLower . trim isLetter $ input
        si = map toLower . trim isLetter $ s

        trim :: (Char -> Bool) -> String -> String
        trim _ ""     = ""
        trim f (x:xs) = if f x  then x : trim f xs
                                else trim f xs

-- | Computes the edit distance between two lists.
editDistance :: Eq a => [a] -> [a] -> Int
editDistance a b  =
    last $ if lab == 0
            then mainDiag
            else if lab > 0
                then lowers !! (lab - 1)
                else{- < 0 -}   uppers !! (-1 - lab)
    where
        mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
        uppers = eachDiag a b (mainDiag : uppers) -- upper diagonals
        lowers = eachDiag b a (mainDiag : lowers) -- lower diagonals

        eachDiag _ []     _                 = []
        eachDiag a (_:bs) (lastDiag:diags)  =
            oneDiag a bs nextDiag lastDiag : eachDiag a bs diags
            where
                nextDiag = head (tail diags)
        eachDiag _ _      _                 = []

        oneDiag a b diagAbove diagBelow = thisdiag
            where
                doDiag []       _        _  _ _ = []
                doDiag _        []       _  _ _ = []
                doDiag (ach:as) (bch:bs) nw n w =
                    me : doDiag as bs me (tail n) (tail w)
                    where
                        me  = if ach == bch
                                then nw
                                else 1 + min3 (head w) nw (head n)
                firstelt    = 1 + head diagBelow
                thisdiag    = firstelt
                            : doDiag a b firstelt diagAbove (tail diagBelow)

        lab = length a - length b
        min3 x y z = if x < y then x else min y z

