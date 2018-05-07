{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | Syntactic sugar for 'MonadFSM' types, adding appropriate row
-- constraints and hiding complexity of the internal implementation.
module Motor.FSM.Sugar
  ( Add(..)
  , Delete(..)
  , To(..)
  , Remain(..)
  , (:->)
  , FromActions
  , NoActions
  , Actions
  , OnlyActions
  , Get
  , type (!-->)
  , type (!+)
  , type (!-)
  ) where

import           Data.Row.Records
import           Data.Kind
import           GHC.TypeLits     (Symbol)

-- | Action that adds a new resource in state 's'.
newtype Add s = Add s

-- | Action that deletes an existing resource in state 's'.
newtype Delete s = Delete s

-- | Action that transitions the state of an existing resource from
-- state 'a' to 'b'.
data To a b = Transition a b

-- | Action where the state of an existing resource remains
-- the same.
data Remain a = Remain a

-- | Mapping from 'Symbol' to some action 'k'.
data (:->) (n :: Symbol) (a :: k)

infixr 5 :->

-- | Translates a list of 'Action's to a 'Row'.
type family FromActions (as :: [*]) (rs :: Row *) (c :: Constraint) :: (Row *, Constraint) where
  FromActions '[] rs c = '( rs, c)
  FromActions ((n :-> Add a) ': ts) r c =
    FromActions ts (Extend n a r) ( c
                                  , (Extend n a r .! n) ~ a
                                  )
  FromActions ((n :-> Delete a) ': ts) r c =
    FromActions ts (r .- n) ( c
                            , (r .! n) ~ a
                            )
  FromActions ((n :-> To a b) ': ts) r c =
    FromActions ts (Modify n b r) ( c
                                  , (r .! n) ~ a
                                  , (Modify n b r .! n) ~ b
                                  )
  FromActions ((n :-> Remain a) ': ts) r c =
    FromActions ts r (c, (r .! n) ~ a)

type NoConstraint = (() :: Constraint)

-- | Alias for 'MonadFSM' that includes no actions.
type NoActions m (r :: Row *) a = m r r a

-- | Alias for 'MonadFSM' that uses 'FromActions' to construct rows.
type Actions m as (i :: Row *) a
   = forall o c. (FromActions as i NoConstraint ~ '( o, c), c) =>
                   m i o a

-- | Alias for 'MonadFSM' that uses 'FromActions' to construct rows,
-- starting from an 'Empty' row, i.e. allowing no /other/ resources.
type OnlyActions m as a = Actions m as Empty a

-- | Gets an existing resource in state 's'.
type Get m (r :: Row *) n = m r r (r .! n)

-- | Infix version of 'To'.
type (!-->) i o = To i o

infixl 6 !-->

-- | Add a named resource. Alias of 'Add'.
type (!+) (n :: Symbol) s = n :-> Add s

infix 6 !+

-- | Delete a named resource. Alias of 'Delete'.
type (!-) (n :: Symbol) s = n :-> Delete s

infix 6 !-
