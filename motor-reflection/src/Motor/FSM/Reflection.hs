{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
-- | Reflect on Motor FSM typeclasses to get state diagram values.
module Motor.FSM.Reflection
  ( Transition(..)
  , Event(..)
  , reflectEvents
  ) where

import           Control.Applicative
import           Data.Foldable
import           Data.Maybe
import           Data.Semigroup
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

import           Motor.FSM.Reflection.Event

data TransitionSigs = TransitionSigs { stateTypeFamily :: Maybe TypeFamilyHead
                                     , transitionSigs  :: [(Name, Type)]
                                     } deriving (Eq, Show)

instance Semigroup TransitionSigs where
  TransitionSigs tf1 s1 <> TransitionSigs tf2 s2 =
    TransitionSigs (tf1 <|> tf2) (s1 <> s2)

instance Monoid TransitionSigs where
  mempty = TransitionSigs Nothing []
  mappend = (<>)

asEvents :: TransitionSigs -> Q [Event]
asEvents sigs = do
  TypeFamilyHead tfName _ _ _ <-
    fail "Missing associated type." `fromMaybe` (return <$> stateTypeFamily sigs)
  concat <$> mapM (sigToTransition tfName) (transitionSigs sigs)

sigToTransition :: Name -> (Name, Type) -> Q [Event]
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
        map (Event (nameBase transitionName)) <$> actionsToTransitions tfName actions
    _ -> fail ("Unsupported type:" ++ show type')

actionsToTransitions :: Name -> Type -> Q [Transition]
actionsToTransitions tfName =
    \case
    AppT (AppT (AppT (AppT (ConT _actions) (VarT _m)) as) (VarT _r)) _ ->
        actionListToTransitions tfName as
    t -> fail ("Unsupported actions type: " ++ show t)

actionListToTransitions :: Name -> Type -> Q [Transition]
actionListToTransitions tfName =
  \case
    AppT (AppT PromotedConsT action) actions ->
      mappend <$> actionToTransitions tfName action <*>
      actionListToTransitions tfName actions
    PromotedNilT  -> return []
    SigT app (AppT ListT _) ->
      actionListToTransitions tfName app
    t -> fail ("Unsupported action type: " ++ show t)

actionToTransitions :: Name -> Type -> Q [Transition]
actionToTransitions tfName =
    \case
    AppT
        (AppT (ConT op) (VarT _n))
        (AppT
        (AppT
        (ConT tf)
        (VarT _m))
        (ConT state))
      | show tf == show tfName ->
        case show op of
          "Motor.FSM.Sugar.!+" -> return [Add (nameBase state)]
          "Motor.FSM.Sugar.!-" -> return [Delete (nameBase state)]
          opS -> fail ("Action infix operator not supported: " ++ opS)

    AppT
        (AppT (PromotedT _assoc) (VarT _n))
        (AppT
        (AppT (ConT _to) (AppT (AppT (ConT tf1) (VarT _m1)) (ConT from)))
        (AppT
        (AppT (ConT tf2) (VarT _m2))
        (ConT to)))
      | show tf1 == show tfName && show tf2 == show tfName ->
      return [Transition (nameBase from) (nameBase to)]
    action -> fail ("Action not supported: " ++ show action)

reflectEvents :: Name -> String -> Q [Dec]
reflectEvents typeClassName (mkName -> defName) = do
  info <- reify typeClassName
  case info of
    ClassI (ClassD _ctx _className _binders _deps decls) _ -> do
      es <- lift =<< asEvents =<< fold <$> mapM getTransitions decls
      return [ SigD defName (AppT ListT (ConT (mkName "Motor.FSM.Reflection.Event.Event")))
             , ValD (VarP defName) (NormalB es) []
             ]
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
