{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Main where

import           Prelude

import           Control.Concurrent     (threadDelay)
import           Control.Monad.Indexed
import           Control.Monad.IO.Class
import           Data.Row.Records
import           GHC.OverloadedLabels

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
  open :: Name n -> Actions m '[ n := State m Closed !--> State m Open ] r ()
  close :: Name n -> Actions m '[ n := State m Open !--> State m Closed ] r ()
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

instance Show (DoorState s) where
  show Open   = "Open"
  show Closed = "Closed"

logLn :: (MonadIO m) => String -> m ()
logLn = liftIO . putStrLn

logDoor :: (MonadIO m, HasType n (DoorState s) i) => Name n -> ConsoleDoor m i i ()
logDoor n =
    get n
    >>>= \s -> logLn ("Door is now " ++ show s)

-- Extremely boring implementation:
instance (MonadIO m) => Door (ConsoleDoor m) where
  type State (ConsoleDoor m) = DoorState
  initial n = new n Closed
  open n = enter n Open >>> logDoor n
  close n = enter n Closed >>> logDoor n
  end = delete

-- * Runner Program

-- This uses the protocol to define a program using the Door protocol.

sleep :: (MonadIO (m i i)) => Int -> m (i :: Row *) (i :: Row *) ()
sleep seconds = liftIO (threadDelay (seconds * 1000000))

confirm :: (MonadIO (m i i)) => String -> m (i :: Row *) (i :: Row *) Bool
confirm s = liftIO (putStrLn s >> ("y" ==) <$> getLine)

type OpenAndClose m n o c =
    ( Door m
    -- TODO: Can these constraints be added to the Sugar module
    -- automatically?
    , ((n .== State m Open) .// c) ~ o
    , ((n .== State m Closed) .// o) ~ c
    , (o .! n) ~ State m Open
    , (c .! n) ~ State m Closed
    , (o .- n) ~ (c .- n)
    )

type OpenAndCloseIO m n o c =
    ( OpenAndClose m n o c
    , MonadIO (m o o)
    , MonadIO (m c c)
    )

inClosed :: OpenAndCloseIO m n o c => Name n -> m c (c .- n) ()
inClosed door = confirm "Open door?" >>>= \case
  True  -> open door >>>= const (inOpen door)
  False -> end door

inOpen :: OpenAndCloseIO m n o c => Name n -> m o (o .- n) ()
inOpen door = confirm "The door must be closed. OK?" >>>= \case
  True  -> close door >>>= const (inClosed door)
  False -> inOpen door

-- The program initializes a door, and starts the looping between
-- open/closed.
main :: IO ()
main = run $ initial #door >>>= const (inClosed #door)

{- $example-run

Running this program can look like this:

>>> main
Open door?
y
Door is now Open
The door must be closed. OK?
y
Door is now Closed
Open door?
y
Door is now Open
The door must be closed. OK?
n
The door must be closed. OK?
n
The door must be closed. OK?
n
The door must be closed. OK?
y
Door is now Closed
Open door?
n
-}
