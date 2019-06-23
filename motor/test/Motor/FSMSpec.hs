{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators    #-}
module Motor.FSMSpec where

import           Prelude               hiding ((>>), (>>=))

import           Control.Monad.Indexed
import           Data.Row.Records

import           Motor.FSM

data S1 = S1
data S2 = S2

n1 :: Name "n1"
n1 = Name

n2 :: Name "n2"
n2 = Name

s1s2 :: MonadFSM m => Name n -> m r ((n .== S2) .// r) ()
s1s2 s = enter s S2

s2s1 :: MonadFSM m => Name n -> m r ((n .== S1) .// r) ()
s2s1 s = enter s S1


dropS1 :: MonadFSM m => Name n -> m r (r .- n) ()
dropS1 = delete

test :: MonadFSM m => m Empty Empty ()
test = do
  new n1 S1
  s1s2 n1
  s2s1 n1

  new n2 S2
  s1s2 n2
  s2s1 n2

  dropS1 n1
  dropS1 n2
  where
    (>>) a = (>>>=) a . const
