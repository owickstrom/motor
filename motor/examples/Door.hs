{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Examples.Door where

import           Prelude                hiding (log)

import           Control.Concurrent     (threadDelay)
import           Control.Monad.Indexed
import           Control.Monad.IO.Class
import           Data.OpenRecords

import           Motor.FSM

-- * Protocol (Abstract state types)

-- We only use marker types for states in the Door protocol.
data Open
data Closed

class MonadFSM m => Door m where
  -- The associated type lets the instance choose the concrete state
  -- data type.
  type State m :: * -> *

  -- Events:
  initial :: Name n -> Actions m '[ n !+ State m Closed ] r ()
  open :: Name n -> Actions m '[ n :-> State m Closed !--> State m Open ] r ()
  close :: Name n -> Actions m '[ n :-> State m Open !--> State m Closed ] r ()
  end :: Name n -> Actions m '[ n !- State m Closed ] r ()

-- * Implemention (Concrete types)
--
-- This could be in another module, hiding the constructors.

newtype ConsoleDoor m (i :: Row *) (o :: Row *) a =
  ConsoleDoor { runConsoleDoor :: FSM m i o a }
  deriving (IxFunctor, IxPointed, IxApplicative, IxMonad, MonadFSM)

run :: Monad m => ConsoleDoor m Empty Empty () -> m ()
run = runFSM . runConsoleDoor

deriving instance Monad m => Functor (ConsoleDoor m i i)
deriving instance Monad m => Applicative (ConsoleDoor m i i)
deriving instance Monad m => Monad (ConsoleDoor m i i)

instance (MonadIO m) => MonadIO (ConsoleDoor m i i) where
  liftIO = ConsoleDoor . liftIO

data DoorState s where
  Open :: DoorState Open
  Closed :: DoorState Closed

-- Extremely boring implementation:
instance (Monad m) => Door (ConsoleDoor m) where
  type State (ConsoleDoor m) = DoorState
  initial n = new n Closed
  open n = enter n Open
  close n = enter n Closed
  end = delete

-- * Runner Program

-- This uses the protocol to define a program using the Door protocol.

sleep :: (MonadIO (m i i)) => Int -> m (i :: Row *) (i :: Row *) ()
sleep seconds =
  liftIO (threadDelay (seconds * 1000000))

confirm :: (MonadIO (m i i)) =>  String -> m (i :: Row *) (i :: Row *) Bool
confirm s =
  liftIO (putStrLn s >> ("y" ==) <$> getLine)

main :: IO ()
main = run prg
  where
    -- A name for our door.
    d :: Name "door"
    d = Name

    -- The program initializes a door, and starts the looping between
    -- open/closed.
    prg = initial d >>>= const (inClosed d)

    -- Type inference is veeeery helpful with these recursive
    -- definitions.
    inClosed door =
      confirm "Open door?" >>>= \case
        True -> open door >>>= const (inOpen door)
        False -> end door
    -- Same here.
    inOpen door =
      confirm "The door must be closed. OK?" >>>= \case
        True -> close door >>>= const (inClosed door)
        False -> inOpen door

{- $example-run

Running this program can look like this:

>>> main
Open door?
y
The door must be closed. OK?
y
Open door?
y
The door must be closed. OK?
n
The door must be closed. OK?
n
The door must be closed. OK?
n
The door must be closed. OK?
y
Open door?
n

-}
