module Bot.Component.Combinator (
    (+++)
,   combine
)   where

import Bot.Component
import Bot.Component.Function()

import Control.Monad.Trans

-- | Combine two generalized process methods into a single process method.
(+++)   ::  (MonadTrans t, Monad (t Bot)) 
        =>  (String -> t Bot ())
        ->  (String -> t Bot ())
        ->  String -> t Bot ()
(+++) first second message  =   first message 
                            >>  second message

-- | Combine a list of generalized process methods into a single process method.
combine ::  (MonadTrans t, Monad (t Bot)) 
        =>  [String -> t Bot ()]
        ->  String -> t Bot ()
combine methods message = mapM_ ($ message) methods
