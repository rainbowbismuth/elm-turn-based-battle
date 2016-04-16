{-
elm-turn-based-battle, a small browser game.
Copyright (C) 2016  Emily A. Bellows

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}


module Main (main) where

import StartApp.Simple as StartApp
import Player exposing (Player(..))
import Class exposing (Class(..))
import Combatant exposing (Combatant)
import Simulation exposing (Simulation)
import Signal exposing (Signal)
import Html exposing (Html)
import UserInterface
import Array


main : Signal Html
main =
  StartApp.start { model = initialModel, view = UserInterface.view, update = UserInterface.update }


initialCombatants : List Combatant
initialCombatants =
  let
    warrior =
      Combatant.mkCombatant Warrior

    thief =
      Combatant.mkCombatant Thief

    cleric =
      Combatant.mkCombatant Cleric
  in
    [ { warrior | player = User, id = 0, name = "Alpha" }
    , { thief | player = User, id = 1, name = "Beta" }
    , { cleric | player = User, id = 2, name = "Gamma" }
    , { warrior | player = AI, id = 3, name = "Delta" }
    , { thief | player = AI, id = 4, name = "Epsilon" }
    , { cleric | player = AI, id = 5, name = "Zeta" }
    ]


initialModel : UserInterface.Model
initialModel =
  { mov = Nothing
  , sim =
      Simulation.clockTickUntilTurn
        { combatLog = []
        , activeCombatant = Nothing
        , combatants = Array.fromList initialCombatants
        }
  }
