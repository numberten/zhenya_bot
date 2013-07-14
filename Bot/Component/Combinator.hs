module Bot.Component.Combinator (
    (+++)
)   where

import Bot.Component
import Bot.Component.Function()

import Control.Monad.Trans

(+++)   ::  (MonadTrans t, Monad (t Bot)) 
        =>  (String -> t Bot ())
        ->  (String -> t Bot ())
        ->  String -> t Bot ()
--(+++) first second = first *> second
(+++) first second message  =   first message 
                            >>  second message

