{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | Syntactic sugar for 'MonadFSM' types.
module Motor.FSM.Sugar
  ( Add(..)
  , Delete(..)
  , To(..)
  , (:->)
  , FromActions
  , NoActions
  , Actions
  , OnlyActions
  , type (!-->)
  , type (!+)
  , type (!-)
  ) where

import           Data.Row.Records
import           GHC.TypeLits     (Symbol)

-- | Action that adds a new resource in state 's'.
newtype Add s = Add s

-- | Action that deletes an existing resource in state 's'.
newtype Delete s = Delete s

-- | Action that transitions the state of an existing resource from
-- state 'a' to 'b'.
data To a b = Transition a b

-- | Mapping from 'Symbol' to some action 'k'.
data (:->) (n :: Symbol) (a :: k)

infixr 5 :->

-- | Translates a list of 'Action's to a 'Row'.
type family FromActions (as :: [*]) (rs :: Row *) :: (Row *) where
  FromActions '[] rs = rs
  FromActions ((n :-> Add a) ': ts) r = Extend n a (FromActions ts r)
  FromActions ((n :-> Delete a) ': ts) r = FromActions ts r .- n
  FromActions ((n :-> To a b) ': ts) r = Modify n b (FromActions ts r)

-- | Alias for 'MonadFSM' that includes no actions.
type NoActions m (r :: Row *) a = m r r a

-- | Alias for 'MonadFSM' that uses 'FromActions' to construct rows.
type Actions m as (r :: Row *) a = m r (FromActions as r) a

-- | Alias for 'MonadFSM' that uses 'FromActions' to construct rows,
-- starting from an 'Empty' row, i.e. allowing no /other/ resources.
type OnlyActions m as a = Actions m as Empty a

-- | Infix version of 'To'.
type (!-->) i o = To i o

infixl 6 !-->

-- | Add a named resource. Alias of 'Add'.
type (!+) (n :: Symbol) s = n :-> Add s

infix 6 !+

-- | Delete a named resource. Alias of 'Delete'.
type (!-) (n :: Symbol) s = n :-> Delete s

infix 6 !-
