{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
-- | Draw state diagrams from reflected events.
module Motor.FSM.Diagram
  ( renderPlantUml
  , renderPlantUmlToFile
  ) where

import           Data.Semigroup
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T

import           Motor.FSM.Reflection.Event

renderPlantUml :: [Event] -> Text
renderPlantUml es = T.unlines ("@startuml" : map draw es ++ ["@enduml"])
  where
    draw (Event (T.pack -> event) transition) =
      case transition of
        Add (T.pack -> state) -> "[*] --> " <> state <> " : " <> event
        Transition (T.pack -> from) (T.pack -> to) ->
          from <> " --> " <> to <> " : " <> event
        Delete (T.pack -> state) -> state <> " --> [*]" <> " : " <> event

renderPlantUmlToFile :: FilePath -> [Event] -> IO ()
renderPlantUmlToFile fp events =
  T.writeFile fp (renderPlantUml events)
