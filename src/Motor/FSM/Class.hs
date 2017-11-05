-- | The 'MonadFSM' class is the indexed monad for finite-state
-- machines.
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Motor.FSM.Class
  ( MonadFSM(..)
  , Name (..)
  ) where

import           Control.Monad.Indexed
import           Data.OpenRecords
import           GHC.TypeLits          (Symbol)

-- * FSM monad

-- | An indexed monad for finite-state machines, managing the state
-- of named resources.
class IxMonad m => MonadFSM (m :: (Row *) -> (Row *) -> * -> *) where
  -- | Creates a new resource and returns its 'Name'.
  new :: Name n -> a -> m r (Extend n a r) ()
  -- | Deletes an existing resource named by its 'Name'.
  delete :: Name n -> m r (r :- n) ()
  -- | Replaces the state of an existing resource named by its 'Name'.
  enter :: Name n -> b -> m r (n ::= b :| (r :- n)) ()
  -- | Run another 'MonadFSM' computation, with empty resource rows,
  -- in this computation.
  call :: m Empty Empty () -> m r r ()

-- | A name of a resource, represented using a 'Symbol'.
data Name (n :: Symbol) where
  Name :: KnownSymbol n => Name n
