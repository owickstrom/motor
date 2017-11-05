{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
-- |
module Motor.FSM.TH where

import           Control.Applicative
import           Data.Foldable
import           Data.Maybe
import           Data.Semigroup
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

data TransitionSigs = TransitionSigs { stateTypeFamily :: Maybe TypeFamilyHead
                                     , transitionSigs  :: [(Name, Type)]
                                     } deriving (Eq, Show)

instance Semigroup TransitionSigs where
  TransitionSigs tf1 s1 <> TransitionSigs tf2 s2 =
    TransitionSigs (tf1 <|> tf2) (s1 <> s2)

instance Monoid TransitionSigs where
  mempty = TransitionSigs Nothing []
  mappend = (<>)

data Transition
  = Add String
  | Transition String
               String
  | Delete String
  deriving (Eq, Show, Lift)

newtype Transitions = Transitions [(String, Transition)]
  deriving (Eq, Show, Lift)

asTransitions :: TransitionSigs -> Q Transitions
asTransitions sigs = do
  TypeFamilyHead tfName _ _ _ <-
    fail "Missing associated type." `fromMaybe` (return <$> stateTypeFamily sigs)
  Transitions . concat <$> mapM (sigToTransition tfName) (transitionSigs sigs)

sigToTransition :: Name -> (Name, Type) -> Q [(String, Transition)]
sigToTransition tfName (transitionName, type') =
    case type' of
    (ForallT
            [ KindedTV _m1 (AppT
                            (AppT
                                ArrowT
                                (AppT
                                (ConT _rowKind1) StarT))
                            (AppT
                                (AppT ArrowT
                                (AppT (ConT _rowKind2) StarT))
                                (AppT (AppT ArrowT StarT) StarT)))
            ]
            [ AppT (ConT _className) (VarT _cm)
            ]
            (ForallT
                [ KindedTV _n1 (ConT _symbolKind1)
                , KindedTV _r1 (AppT (ConT _rowKind3) StarT)
                ]
                _constraints
                (AppT _ actions))) ->
        map (nameBase transitionName,) <$> actionsToTransitions tfName actions
    _ -> do
        reportWarning ("Unsupported type:" ++ show type')
        return []

actionsToTransitions :: Name -> Type -> Q [Transition]
actionsToTransitions tfName =
    \case
    SigT (AppT (AppT (AppT (AppT (ConT _actions) (VarT _m)) as) (VarT _r)) _) _ ->
        actionListToTransitions tfName as
    _ -> return []

actionListToTransitions :: Name -> Type -> Q [Transition]
actionListToTransitions tfName =
    \case
    SigT
        (AppT
        (AppT
            PromotedConsT
            action)
        actions)
        (AppT ListT StarT) ->
        mappend
        <$> actionToTransitions tfName action
        <*> actionListToTransitions tfName actions
    SigT PromotedNilT (AppT ListT StarT) ->
        return []
    t -> do
        reportWarning ("Unsupported action type: " ++ show t)
        return []

actionToTransitions :: Name -> Type -> Q [Transition]
actionToTransitions tfName =
    \case
    AppT
        (AppT (ConT _add) (VarT _n))
        (AppT
        (AppT
        (ConT tf)
        (VarT _m))
        (ConT state))
      | show tf == show tfName ->
        return [Add (nameBase state)]

    AppT
        (AppT (ConT _assoc) (VarT _n))
        (AppT
        (AppT (ConT _to) (AppT (AppT (ConT tf1) (VarT _m1)) (ConT from)))
        (AppT
        (AppT (ConT tf2) (VarT _m2))
        (ConT to)))
      | show tf1 == show tfName && show tf2 == show tfName ->
      return [Transition (nameBase from) (nameBase to)]
    action -> fail ("Action not supported: " ++ show action)

showClass :: Name -> Q [Dec]
showClass name = do
  info <- reify name
  case info of
    ClassI (ClassD _ctx _className _binders _deps decls) _ -> do
      ts <- asTransitions =<< fold <$> mapM getTransitions decls
      [d|
        transitions :: Transitions
        transitions = ts
        |]
    _ ->
      fail "Not an FSM typeclass."

  where
    getTransitions :: Dec -> Q TransitionSigs
    getTransitions dec =
      case dec of
        OpenTypeFamilyD tf@(TypeFamilyHead _n _ _ _) ->
          return mempty { stateTypeFamily = Just tf }
        SigD n t ->
          return mempty { transitionSigs = [(n, t)]}
        _ -> return mempty
