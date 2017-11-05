{-# LANGUAGE DeriveLift #-}
-- | Value-level representations of FSMs.
module Motor.FSM.Reflection.Event where

import           Language.Haskell.TH.Syntax (Lift)

data Transition
  = Add String
  | Transition String
               String
  | Delete String
  deriving (Eq, Show, Lift)

data Event = Event String Transition
  deriving (Eq, Show, Lift)
