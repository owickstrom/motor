-- | Template Haskell wrapper for "Motor.FSM.Diagram" module.
module Motor.FSM.Diagram.TH where

import           Language.Haskell.TH

import qualified Motor.FSM.Diagram          as Diagram
import           Motor.FSM.Reflection.Event

renderPlantUmlToFile :: FilePath -> [Event] -> Q [Dec]
renderPlantUmlToFile fp events = do
  runIO (Diagram.renderPlantUmlToFile fp events)
  return []
