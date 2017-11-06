{-# LANGUAGE TemplateHaskell #-}
module GenerateDiagram where

import           Motor.FSM.Diagram.TH       (renderPlantUmlToFile)
import           Motor.FSM.Reflection.Event

$(renderPlantUmlToFile
    "diagrams/game.uml.txt"
    [ Event "spawn" (Add "Standing")
    , Event "jump" (Transition "Standing" "Jumping")
    , Event "land" (Transition "Jumping" "Standing")
    , Event "perish" (Delete "Standing")
    ])
