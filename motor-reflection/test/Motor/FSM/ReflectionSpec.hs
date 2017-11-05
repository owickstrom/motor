{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Motor.FSM.ReflectionSpec where

import           Prelude                    hiding (log, (>>))

import           Data.OpenRecords
import           Test.Hspec

import           Motor.FSM
import           Motor.FSM.Reflection
import           Motor.FSM.Reflection.Event

-- * Game Protocol/Machine

data Standing
data Jumping

class MonadFSM m => Game (m :: Row * -> Row * -> * -> *) where
  type State m :: * -> *
  spawn
    :: KnownSymbol n
    => Name n
    -> Actions m '[n !+ State m Standing] r ()
  jump
    :: KnownSymbol n
    => Name n
    -> Actions m '[n :-> State m Standing !--> State m Jumping] r ()
  land
    :: KnownSymbol n
    => Name n
    -> Actions m '[n :-> State m Jumping !--> State m Standing] r ()
  perish
    :: KnownSymbol n
    => Name n
    -> Actions m '[n !- State m Standing] r ()

reflectEvents ''Game "gameEvents"

spec :: Spec
spec =
  it "reflects events when using a State type family" $
    gameEvents `shouldBe` [ Event "spawn" (Add "Standing")
                          , Event "jump" (Transition "Standing" "Jumping")
                          , Event "land" (Transition "Jumping" "Standing")
                          , Event "perish" (Delete "Standing")
                          ]
