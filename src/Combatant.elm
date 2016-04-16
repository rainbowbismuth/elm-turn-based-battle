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


module Combatant (..) where

import Id exposing (Id)
import Player exposing (Player(..))
import Move exposing (Move)
import Class exposing (Class(..))


type State
  = Default
  | Defending


type alias Combatant =
  { id : Id
  , player : Player
  , name : String
  , class : Class
  , hitPoints : Float
  , actionPoints : Int
  , chargeTime : Int
  , state : State
  }


mkCombatant : Class -> Combatant
mkCombatant cls =
  let
    default =
      { id = 0
      , player = User
      , name = "missingname"
      , class = Warrior
      , hitPoints = 0.0
      , actionPoints = 0
      , chargeTime = 0
      , state = Default
      }
  in
    case cls of
      Warrior ->
        { default | class = cls, hitPoints = 100 }

      Thief ->
        { default | class = cls, hitPoints = 70 }

      Cleric ->
        { default | class = cls, hitPoints = 80 }


strength : Combatant -> Float
strength cmbt =
  Class.strength cmbt.class


defense : Combatant -> Float
defense cmbt =
  Class.defense cmbt.class * defenseBonus cmbt


defenseBonus : Combatant -> Float
defenseBonus cmbt =
  case cmbt.state of
    Defending ->
      0.5

    _ ->
      1


speed : Combatant -> Int
speed cmbt =
  Class.speed cmbt.class


moveList : Combatant -> List Move
moveList cmbt =
  Class.moveList cmbt.class


moveAvailable : Move -> Combatant -> Bool
moveAvailable move cmbt =
  List.member move (moveList cmbt)


alive : Combatant -> Bool
alive cmbt =
  cmbt.hitPoints > 0


dead : Combatant -> Bool
dead cmbt =
  not (alive cmbt)


friendsOf : Player -> Combatant -> Bool
friendsOf player cmbt =
  cmbt.player == player


foesOf : Player -> Combatant -> Bool
foesOf player cmbt =
  cmbt.player /= player


friends : Combatant -> Combatant -> Bool
friends cmbtA cmbtB =
  cmbtA.player == cmbtB.player


foes : Combatant -> Combatant -> Bool
foes cmbtA cmbtB =
  cmbtA.player /= cmbtB.player


canHaveActiveTurn : Combatant -> Bool
canHaveActiveTurn cmbt =
  alive cmbt && cmbt.chargeTime >= 100


clockTick : Combatant -> Combatant
clockTick cmbt =
  if alive cmbt then
    { cmbt | chargeTime = cmbt.chargeTime + speed cmbt }
  else
    cmbt


increaseAP : Combatant -> Combatant
increaseAP cmbt =
  { cmbt | actionPoints = min (cmbt.actionPoints + 1) 5 }


payAP : Int -> Combatant -> Maybe Combatant
payAP amount cmbt =
  if cmbt.actionPoints >= amount then
    Just { cmbt | actionPoints = cmbt.actionPoints - amount }
  else
    Nothing


payTurnCT : Combatant -> Combatant
payTurnCT cmbt =
  { cmbt | chargeTime = cmbt.chargeTime - 100 }


toDefaultState : Combatant -> Combatant
toDefaultState cmbt =
  { cmbt | state = Default }


toDefendState : Combatant -> Combatant
toDefendState cmbt =
  { cmbt | state = Defending }
