{-# LANGUAGE TemplateHaskell #-}
module GenerateDiagram where

import           Motor.FSM.Diagram          (renderPlantUml)
import           Motor.FSM.Diagram.TH       (renderPlantUmlToFile)
import           Motor.FSM.Reflection.Event

-- Merely compiling this file will generate the PlantUML diagram to
-- @diagrams/game.uml.txt@. It then has to be run through the PlantUML
-- JAR file to generate PNG, PDF, SVG, etc.

$(renderPlantUmlToFile
    "diagrams/game.uml.txt"
    [ Event "spawn" (Add "Standing")
    , Event "jump" (Transition "Standing" "Jumping")
    , Event "land" (Transition "Jumping" "Standing")
    , Event "perish" (Delete "Standing")
    ])
