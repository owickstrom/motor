-- | The 'MonadFSM' class is the indexed monad for finite-state
-- machines.
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
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
import           Data.Row.Records
import           GHC.OverloadedLabels
import           GHC.TypeLits          (Symbol)

-- * FSM monad

-- | An indexed monad for finite-state machines, managing the state
-- of named resources.
class IxMonad m => MonadFSM (m :: (Row *) -> (Row *) -> * -> *) where
  -- | Creates a new resource by name.
  new :: Name n -> a -> m r (Extend n a r) ()
  -- | Returns an existing resource.
  get :: HasType n a r => Name n -> m r r a
  -- | Deletes an existing resource named by its 'Name'.
  delete :: Name n -> m r (r .- n) ()
  -- | Replaces the state of an existing resource named by its 'Name'.
  enter
    :: ( r' ~ (r .// (n .== b))
       )
    => Name n
    -> b
    -> m r r' ()
  -- | Updates the state, using a pure function, of an existing
  -- resource named by its 'Name'.
  update :: (HasType n a r, Modify n a r ~ r) => Name n -> (a -> a) -> m r r ()
  -- | Embed another 'MonadFSM' computation, with empty resource rows,
  -- in this computation.
  call :: m Empty Empty a -> m r r a

-- | A name of a resource, represented using a 'Symbol'.
data Name (n :: Symbol) where
  Name :: KnownSymbol n => Name n

instance (KnownSymbol n, n ~ n') => IsLabel n (Name n') where
  fromLabel = Name :: Name n'
