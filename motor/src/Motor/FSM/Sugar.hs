{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LiberalTypeSynonyms   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | Syntactic sugar for 'MonadFSM' types, adding appropriate row
-- constraints and hiding complexity of the internal implementation.
module Motor.FSM.Sugar
  ( Action(..)
  , ActionMapping(..)
  , FromActions
  , NoActions
  , Actions
  , OnlyActions
  , Get
  , type (!-->)
  , type (!+)
  , type (!-)
  ) where

import           Data.Kind
import           Data.Row.Records
import           GHC.TypeLits     (Symbol)

data Result = Error | Ok

data Resource (result :: Result) where
  ResourceOk :: r -> Resource 'Ok
  ResourceError :: e -> Resource 'Error

-- | An 'Action' describes a resource action.
data Action
  = Add (forall r. Resource r)
  -- ^ Adds a new resource of the given 'Resource'.
  | Remain (forall r. Resource r)
  -- ^ The existing resource of the given 'Resource' remains the same.
  | To (forall r. Resource r)
       (forall r. Resource r)
  -- ^ Transitions an existing resource from the first 'Resource r' to a
  -- resource of the second 'Resource'.
  | Delete (forall r. Resource r)
  -- ^ Deletes an existing resource of the given 'Resource'.

-- | Mapping from 'Symbol' to some action 'a'.
data ActionMapping = (:=) Symbol Action

infixr 5 :=

-- | Translates a list of 'Action's to a 'Row'.
type family FromActions
  (as :: [ActionMapping])
  (rs :: Row (Resource (r :: Result)))
  (c :: Constraint)
  :: (Row (Resource (r :: Result)), Constraint) where
  FromActions '[] rs c = '( rs, c)
  FromActions ((n ':= 'Add a) ': ts) r c =
    FromActions ts (Extend n a r) ( c
                                  , (Extend n a r .! n) ~ a
                                  )
  FromActions ((n ':= 'Delete a) ': ts) r c =
    FromActions ts (r .- n) ( c
                            , (r .! n) ~ a
                            )
  FromActions ((n ':= 'To a b) ': ts) r c =
    FromActions ts (Modify n b r) ( c
                                  , (r .! n) ~ a
                                  , (Modify n b r .! n) ~ b
                                  )
  FromActions ((n ':= 'Remain a) ': ts) r c =
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
type (!-->) i o = 'To i o

infixl 6 !-->

-- | Add a named resource. Alias of 'Add'.
type (!+) (n :: Symbol) s = n ':= 'Add s

infix 6 !+

-- | Delete a named resource. Alias of 'Delete'.
type (!-) (n :: Symbol) s = n ':= 'Delete s

infix 6 !-
