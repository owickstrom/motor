{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{- | /Motor/ is an experimental Haskell library for building
   finite-state machines with type-safe transitions and effects. It
   draws inspiration from the Idris
   [ST](http://docs.idris-lang.org/en/latest/st/state.html) library.
-}
module Motor.FSM (

  -- * Usage
  -- $usage

  -- ** Indexed Monads
  -- $indexed-monads

  -- ** State Actions
  -- $state-actions

  -- ** Infix Operators
  -- $infix

  -- ** Row Polymorphism
  -- $row-polymorphism

  -- * Examples
  -- $examples

  -- * API

  -- ** MonadFSM Class
  MonadFSM (..)

  -- ** Resource Names
  , Name (..)

  -- ** State Actions
  , (:->)
  , To, Add, Delete, Remain
  , FromActions, NoActions, Actions , OnlyActions
  , Get

  -- ** Aliases
  , type (!-->)
  , type (!+)
  , type (!-)

  -- ** FSM
  , FSM, runFSM

  -- * Indexed Monad Utilities
  , (>>>)

  -- * Re-exports
  , module Control.Monad.Indexed
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Indexed
import           Control.Monad.Indexed.State
import           Control.Monad.Indexed.Trans
import           Data.Functor.Identity       (runIdentity)
import           Data.Row.Records

import           Motor.FSM.Class
import           Motor.FSM.Sugar


-- | IxStateT-based implementation of 'MonadFSM'.
newtype FSM m (i :: Row *) (o :: Row *) a =
  FSM (IxStateT m (Rec i) (Rec o) a)

instance Monad m => IxFunctor (FSM m) where
  imap f (FSM ma) = FSM (imap f ma)

instance Monad m => IxPointed (FSM m) where
  ireturn a = FSM (ireturn a)

instance Monad m => IxApplicative (FSM m) where
  iap (FSM f) (FSM a) = FSM (iap f a)

instance Monad m => IxMonad (FSM m) where
  ibind f (FSM ma) =
    FSM
      (ibind
         (\a ->
            let FSM b = f a
            in b)
         ma)

instance Monad m => Functor (FSM m i i) where
  fmap = imap

instance Monad m => Applicative (FSM m i i) where
  pure = ireturn
  (<*>) = iap

instance Monad m => Monad (FSM m i i) where
  return = ireturn
  m >>= k = ibind k m

instance IxMonadTrans FSM where
  ilift = FSM . ilift

instance MonadIO m => MonadIO (FSM m i i) where
  liftIO = ilift . liftIO

-- | Run an 'FSM' state machine and retrieve its return value. Note
-- that all resources added in the 'FSM' computation must be deleted
-- eventually, as the output row is 'Empty'.
runFSM :: Monad m => FSM m Empty Empty a -> m a
runFSM (FSM f) = fst <$> runIxStateT f empty

instance Monad m => MonadFSM (FSM m) where
  new (Name :: Name n) x = FSM (imodify (extend lbl x))
    where
      lbl = Label :: Label n
  get (Name :: Name n) = FSM (igets (.! lbl))
    where
      lbl = Label :: Label n
  delete (Name :: Name n) = FSM (imodify (.- lbl))
    where
      lbl = Label :: Label n
  update (Name :: Name n) f = FSM (imodify $ \s -> runIdentity (focus lbl (pure . f) s))
    where
      lbl = Label :: Label n
  enter (Name :: Name n) x = FSM (imodify $ \s -> runIdentity (focus lbl (const (pure x)) s))
    where
      lbl = Label :: Label n
  call (FSM ma) = FSM (ilift (fst <$> runIxStateT ma empty))

{- $usage
The central finite-state machine abstraction in Motor is the 'MonadFSM' type class.
'MonadFSM' is an /indexed monad/ type class, meaning that it has not one,
but /three/ type parameters:

  1. A 'Row' of input resource states
  2. A 'Row' of output resource state
  3. A return type (just as in 'Monad')

The 'MonadFSM' parameter kinds might look a bit scary, but they state
the same:

@
class IxMonad m => MonadFSM (m :: (Row *) -> (Row *) -> * -> *) where
  ...
@

The rows describe how the FSM computation will affect the state of its
resources when evaluated. A row is essentially a type-level map, from
resource names to state types, and the FSM computation's rows describe
the resource states /before/ and /after/ the computation.

An FSM computation @newConn@ that adds a resource named @"connection"@
with state @Idle@ could have the following type:

>>> :t newConn
newConn :: MonadFSM m => m r ("connection" ::= Idle :| r) ()

A computation @spawnTwoPlayers@ that adds two resources could have
this type:

>>> :t spawnTwoPlayers
spawnTwoPlayers ::
  :: MonadFSM m =>
     m r ("hero2" ::= Standing :| "hero1" ::= Standing :| r) ()

Motor uses the extensible records in "Data.OpenRecords", provided by
the [CTRex](https://wiki.haskell.org/CTRex) library, for row kinds.
Have a look at it's documentation to learn more about the type-level
operators available for rows.


-}

{- $indexed-monads

As mentioned above, 'MonadFSM' is an indexed monad. It uses the
definition from "Control.Monad.Indexed", in the
[indexed](https://hackage.haskell.org/package/indexed-0.1.3)
package. This means that you can use 'ibind' and friends to compose
FSM computations.

@
c1 >>>= \\_ -> c2
@

You can combine this with the @RebindableSyntax@ language extension to
get do-syntax for FSM programs:

@
test :: MonadFSM m => m Empty Empty ()
test = do
  c1
  c2
  r <- c3
  c4 r
  where
    (>>) a = (>>>=) a . const
    (>>=) = (>>>=)
@

See [24 Days of GHC Extensions: Rebindable
 Syntax](https://ocharles.org.uk/blog/guest-posts/2014-12-06-rebindable-syntax.html)
 for some more information on how to use @RebindableSyntax@.

-}

(>>>) :: IxMonad m => m i j a -> m j k b -> m i k b
(>>>) a = (>>>=) a . const

{- $state-actions
To make it easier to read and write FSM computation types, there is
some syntax sugar available.

/State actions/ allow you two describe state changes of named
resources with a /single/ list, as opposed two writing two rows. They
also take care of matching the CTRex row combinators with the
expectations of Motor, which can be tricky to do by hand.

There are three state actions:

* 'Add' adds a new resource.
* 'To' transitions the state of a resource.
* 'Delete' deletes an existing resource.

A mapping between a resource name is written using the `:->` type operator,
with a `Symbol` on the left, and a state action type on the right. Here are
some examples:

@
"container" :-> Add Empty

"list" :-> To Empty NonEmpty

"game" :-> Delete GameEnded
@

So, the list of mappings from resource names to state actions describe
what happens to each resource. Together with an initial row of
resources 'r', and a return value 'a', we can declare the type of an
FSM computation using the 'Actions' type:

@
MonadFSM m => Actions m '[ n1 :-> a1, n2 :-> a2, ... ] r a
@

A computation that adds two resources could have the following type:

@
addingTwoThings ::
  MonadFSM m =>
  Actions m '[ "container" :-> Add Empty, "game" :-> Add Started ] r ()
@
-}

{- $infix As an alternative to the 'Add', 'To', and 'Delete' types,
Motor offers infix operator aliases. These start with @!@ to indicate
that they can be effectful.

The '!-->' operator is an infix alias for `To`:

@
useStateMachines ::
  MonadFSM m =>
  Actions m '[ "program" :-> NotCool !--> Cool ] r ()
@

The `!+` and `!-` are infix aliases for mappings from resource names to `Add`
and `Delete` state actions, respectively:

@
startNewGame ::
  MonadFSM m =>
  Actions m '[ "game" !+ Started ] r ()

endGameWhenWon ::
  MonadFSM m =>
  Actions m '[ "game" !- Won ] r ()
@
-}

{- $row-polymorphism

Because of how CTRex works, FSM computations that have a free variable as
their input row of resources, i.e. that are polymorphic in the sense of
other resource states, must list /all their actions in reverse order/.

@
doFourThings ::
     Game m
  => Actions m '[ "hero2" !- Standing
                , "hero1" !- Standing
                , "hero2" !+ Standing
                , "hero1" !+ Standing
                ] r ()
doFourThings =
  spawn hero1
  >>>= \_ -> spawn hero2
  >>>= \_ -> perish hero1
  >>>= \_ -> perish hero2
@

Had the @r@ been replaced by @Empty@ in the type signature above, it could
have had type @NoActions m Empty ()@ instead.

If the computation removes all resources that it creates, i.e. that it
could be run as @NoActions m Empty ()@, you can use 'call' to run it
in a row-polymorphic computation without having to list all actions:

@
doFourThings ::
     Game m
  => NoActions m r ()
doFourThings = call $
  spawn hero1
  >>>= \_ -> spawn hero2
  >>>= \_ -> perish hero1
  >>>= \_ -> perish hero2
@

In a future version, 'call' might support the rows of the called
computation being subsets of the resulting computation's rows.
-}


{- $examples

The [GitHub repository](https://github.com/owickstrom/motor) includes
some examples, check that out.
-}
