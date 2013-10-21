module Bot.Component.Combinator (
    (+++)
,   (>>+)
,   combine
)   where

import Bot.Component

import Control.Applicative
import Control.Monad.Trans

-- | Combine two generalized process methods into a single process method.
(+++)   ::  BotMonad b
        =>  (String -> b ())
        ->  (String -> b ())
        ->  String -> b ()
(+++) first second message  =   first message
                            >>  second message

-- | Combine a list of generalized process methods into a single process method.
combine ::  BotMonad b
        =>  [String -> b ()]
        ->  String -> b ()
combine methods message = mapM_ ($ message) methods

-- | A convenient combinator for stacking multiple `ComponentPart`s.
(>>+)    ::  (BotMonad b, BotMonad (d b), Applicative (d b), MonadTrans d)
         =>  Bot (ComponentPart b)
         ->  (BotExtractor b -> Bot (ComponentPart (d b)))
         ->  Bot (ComponentPart (d b))
innerComponent >>+ outerComponentCreator = do
    (innerExtractor, innerAction)   <-  innerComponent
    (outerExtractor, outerAction)   <-  outerComponentCreator innerExtractor
    let combinedAction = (*>) <$> lift . innerAction <*> outerAction
    return (outerExtractor, combinedAction)
