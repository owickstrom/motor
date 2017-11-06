{-# LANGUAGE OverloadedStrings #-}
module Motor.FSM.DiagramSpec where

import           Prelude                    hiding (log, (>>))

import qualified Data.Text                  as T
import           Test.Hspec

import           Motor.FSM.Diagram          (renderPlantUml)
import           Motor.FSM.Reflection.Event

spec :: Spec
spec = do
  let testEvents =
        [ Event "spawn" (Add "Standing")
        , Event "jump" (Transition "Standing" "Jumping")
        , Event "land" (Transition "Jumping" "Standing")
        , Event "perish" (Delete "Standing")
        ]
  it "renders events as a PlantUML state diagram" $
    renderPlantUml testEvents `shouldBe`
    T.unlines
      [ "@startuml"
      , "[*] --> Standing : spawn"
      , "Standing --> Jumping : jump"
      , "Jumping --> Standing : land"
      , "Standing --> [*] : perish"
      , "@enduml"
      ]
