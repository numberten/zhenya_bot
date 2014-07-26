How to Roll Your Own Component
==============================

This tutorial will walk you through the steps required to implement a new
zhenya_bot feature. Specfically, we will be going through the process of
creating the reputation component described in #46. The resulting component will
keep track of the total reputation a user has, and allow other users to grant
reputation via the `+1` command.

Component Basics
----------------

New features are introduced to zhenya_bot via a simple plugin mechanism.
Plugins in zhenya_bot terminology are called "Components", and they are an
integral aspect of zhenya_bot's architecture. Each component encapsulates a
single responsibility or feature. For example, there is a component to handle
responding to PING messages, and another to echo the title of Youtube videos
posted in a channel.

Some components also provide a service to other components. We will be using
such a component for getting a list of the last messages spoken in a channel.

The first step in creating a new component is to create a module in
Bot.Component.Impl. The simplest possible component we can define is:

    module Bot.Component.Impl.Reputation (
        reputation
    )   where

    import Bot.Component

    reputation :: Bot Component
    reputation = mkComponent $ \_ -> return ()

Then for the component to actually be included it must be added to Main.hs:

    ...
    import Bot.Component.Impl.Reputation
    ...

    main :: IO ()
    main = do
        ...
        runBot $ defaultBotConfig {
                ...
            } `withComponents` [
                ...
            ,   reputation
                ...
            ]

This snippet defines the `Bot.Component.Impl.Reputation` module, and exports the
`reputation` function. There is a lot going on in the definition of the
`reputation` function, so let's take a second to digest what is actually
happening here.

First up, the type of `reputation` is `Bot Component`, that is it returns some
type `Component` wrapped in the `Bot` monad. The `Bot` monad is basically just a
state monad transformer on top of the IO monad. If that doesn't make any sense,
don't worry, you can just think of `Bot` as monad/type that lets you interact
with the IRC server.

The `Component` type is unsurprisingly the type for components. Values of the
`Component` type can be constructed via the `mkComponent` function. The type of
`mkComponent` is a little hairy, so for now let us just say that `mkComponent`
takes "something that looks like a component", and returns a component. In this
example, the "something that looks like a component" is something of the type
`String -> Bot ()`, so something that takes an IRC message and returns some `Bot
()` value.

Component Combinators
---------------------

Currently, our component doesn't do anything interesting and is pretty boring.
Let's change that!

This section will introduce you to some of the more common component combinators
that zhenya_bot provides to component writers. There are component combinators
for common tasks like handling commands, and maintaining state. Combinators also
provide a mechanism for building up a component of smaller pieces.

The first combinator that you will be introduced to is the `simpleCommand`
combinator. The type of `simpleCommand` is `String -> Bot () -> Bot Component`.
The `simpleCommand` combinator takes a string argument for the command to
trigger the component, and a `Bot ()` that performs an action in the `Bot`
monad. Let's see a simple example.

    module Bot.Component.Impl.Reputation (
        reputation
    )   where

    import Bot.Component
    import Bot.Component.Command
    import Bot.IO

    reputation :: Bot Component
    reputation = simpleCommand "+1" $ ircReply "That was awesome!"

This snippet will cause `zhenya_bot` to reply the message "That was awesome!"
whenever someone in the channel says "+1".

Being able to respond to a command is cool and all, but right now there is no
memory of previous times the command was run. The family of stateful combinators
provides a mechanism for maintaining state across component invocations. The
following example keeps track of the number of times the `+1` command has been
invoked.

    module Bot.Component.Impl.Reputation (
        reputation
    )   where

    import Bot.Component
    import Bot.Component.Combinator
    import Bot.Component.Command
    import Bot.Component.Stateful
    import Bot.IO

    import Control.Monad.State

    reputation :: Bot Component
    reputation = stateful (plusOne +++ howMany) initialState

    initialState :: Bot Int
    initialState = return 0

    plusOne :: BotMonad b => String -> StateT Int b ()
    plusOne = simpleCommandT "+1" $ do
        modify (+ 1)
        liftBot $ ircReply "Noted."

    howMany :: BotMonad b => String -> StateT Int b ()
    howMany = simpleCommandT "?" $ do
        count <- get
        liftBot $ ircReply $ "I've seen " ++ show count ++ " awesome things!"

There is a lot packed into this example, let's break down what is happening
here.

First off, note that now instead of using the `simpleCommand` combinator we are
now using the `simpleCommandT` combinator. The difference between the two is
that instead of directly returning a `Bot Component`, it returns something that
can be composed with other combinators. Specifically, it returns a `BotMonad b
=> String -> b()`. `BotMonad` is a type class that represents a monad
transformer stack with a `Bot` monad at the root. Many of `zhenya_bot`'s
combinators have both a plain and a 'T' suffixed version.

Secondly, note that we were able to glue together two commands into a single
action using the `+++` combinator. This is possible because `simpleCommandT`
returns a `BotMonad b => String -> b ()`. Also note that both commands share the
same state.

Using Services
--------------

While most components correspond to user facing features, there are a handful of
components that provide services to other components. For example, there is a
component that keeps track of everything that was said in a channel, and one
that keeps track of which nicks a user has assumed.

All of these service components operate in a similar manner. They export an
opaque handle type, and all of the services APIs take this handle type as their
first parameter. A value of this type is created when the service component is
created. Any component that wishes to use a service must have this handle value.

For example, lets see how to get a list of all messages that zhenya_bot has
seen.

Bot/Component/Impl/Reputation.hs:

    module Bot.Component.Impl.Reputation (
        reputation
    )   where

    import Bot.Component
    import Bot.Component.Command
    import Bot.Component.Impl.History
    import Bot.IO

    import Control.Monad.Trans.Identity
    import qualified Data.Sequence as S

    reputation :: HistoryHandle -> Bot Component
    reputation historyHandle =
            mkComponentT $ simpleCommandT "+1" (plusOne historyHandle)

    plusOne :: HistoryHandle -> IdentityT Bot ()
    plusOne historyHandle = do
        history <- getHistory historyHandle
        let secondToLastNick = fst $ history `S.index` (S.length history - 2)
        liftBot $ ircReply $ "Hey " ++ secondToLastNick ++ ", that was sick!"

Main.hs:

    ...
    import Bot.Component.Impl.Reputation
    ...

    main :: IO ()
    main = do
        ...
        runBot $ defaultBotConfig {
                ...
            } `withComponents` [
                ...
            ,   reputation historyHandle
                ...
            ]

Putting it all together
-----------------------

By now you have seen everything required to build a component that keeps track
of user reputation. Putting all the pieces together is left as an exercise for
the reader. If you get stuck, you can view the full implementation of the
reputation component [here](
https://github.com/numberten/zhenya_bot/blob/master/Bot/Component/Impl/Reputation.hs).
