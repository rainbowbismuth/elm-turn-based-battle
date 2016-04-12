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


module Simulation (..) where

import Array exposing (Array)
import Player exposing (Player(..))
import Combatant exposing (Combatant)
import Command exposing (Command(..))
import Move exposing (Move(..))
import Id exposing (Id)
import Maybe
import Debug


type alias Simulation =
  { combatants : Array Combatant
  , activeCombatant : Maybe Int
  , combatLog : List String
  }


combatants : Simulation -> Array Combatant
combatants sim =
  sim.combatants


lost : Player -> Simulation -> Bool
lost player sim =
  let
    reducer cmbt x =
      if cmbt.player == player then
        x && Combatant.dead cmbt
      else
        x
  in
    Array.foldr reducer True sim.combatants


gameOver : Simulation -> Bool
gameOver sim =
  lost AI sim || lost User sim


party : Player -> Simulation -> Array Combatant
party player sim =
  Array.filter (\cmbt -> cmbt.player == player) sim.combatants


enemies : Player -> Simulation -> Array Combatant
enemies player sim =
  Array.filter (\cmbt -> cmbt.player /= player) sim.combatants


findActiveCmbt : Simulation -> Maybe Combatant
findActiveCmbt sim =
  let
    recur i =
      case Array.get i sim.combatants of
        Just cmbt ->
          if Combatant.canHaveActiveTurn cmbt then
            Just cmbt
          else
            recur (i + 1)

        Nothing ->
          Nothing
  in
    recur 0


activeCmbt : Simulation -> Maybe Combatant
activeCmbt sim =
  case sim.activeCombatant of
    Just id ->
      Just (combatantByIdMustExist id sim)

    Nothing ->
      Nothing


activeCmbtMustExist : Simulation -> Combatant
activeCmbtMustExist sim =
  case activeCmbt sim of
    Just cmbt ->
      cmbt

    Nothing ->
      Debug.crash "this should not be possible"


doIHaveActiveTurn : Id -> Simulation -> Bool
doIHaveActiveTurn id sim =
  case activeCmbt sim of
    Just cmbt ->
      cmbt.id == id

    Nothing ->
      False


dropActiveTurn : Simulation -> Simulation
dropActiveTurn sim =
  let
    cmbt =
      activeCmbtMustExist sim

    nextCmbt =
      Combatant.payTurnCT cmbt
  in
    clockTickUntilTurn
      { sim | combatants = Array.set cmbt.id nextCmbt sim.combatants, activeCombatant = Nothing }


clockTickUntilTurn : Simulation -> Simulation
clockTickUntilTurn initialSim =
  let
    recur sim =
      case findActiveCmbt sim of
        Just cmbt ->
          let
            nextCmbt =
              Combatant.increaseAP cmbt

            nextCombatants =
              Array.set nextCmbt.id nextCmbt sim.combatants
          in
            { sim | activeCombatant = Just cmbt.id, combatants = nextCombatants }

        Nothing ->
          recur (clockTick sim)
  in
    case initialSim.activeCombatant of
      Just _ ->
        initialSim

      Nothing ->
        recur initialSim


whosTurn : Simulation -> Maybe Player
whosTurn sim =
  activeCmbt sim
    |> Maybe.map (\cmbt -> cmbt.player)


clockTick : Simulation -> Simulation
clockTick sim =
  { sim | combatants = Array.map Combatant.clockTick sim.combatants }


turnOrderList : Simulation -> List Combatant
turnOrderList initialSim =
  let
    recur i sim =
      if i > 0 then
        case activeCmbt sim of
          Just cmbt ->
            cmbt :: recur (i - 1) (clockTickUntilTurn (dropActiveTurn sim))

          Nothing ->
            recur (i - 1) (clockTickUntilTurn sim)
      else
        []
  in
    recur 12 initialSim


combatantById : Id -> Simulation -> Maybe Combatant
combatantById id sim =
  Array.get id sim.combatants


combatantByIdMustExist : Id -> Simulation -> Combatant
combatantByIdMustExist id sim =
  case combatantById id sim of
    Just cmbt ->
      cmbt

    Nothing ->
      Debug.crash "combatant with this ID must exist!"


modifyById : (Combatant -> Combatant) -> Id -> Simulation -> Simulation
modifyById f id sim =
  case Array.get id sim.combatants of
    Just cmbt ->
      { sim | combatants = Array.set id (f cmbt) sim.combatants }

    Nothing ->
      sim


existsAndAlive : Id -> Simulation -> Bool
existsAndAlive id sim =
  case combatantById id sim of
    Just cmbt ->
      Combatant.alive cmbt

    Nothing ->
      False


targetReaction : Combatant -> Move -> Combatant -> ( Combatant, List String )
targetReaction user mv target =
  case mv of
    Attack ->
      let
        dmg =
          (ceiling >> toFloat) (Combatant.strength user * Combatant.defense target * 20)

        msg =
          user.name ++ " deals " ++ (toString dmg) ++ " damage to " ++ target.name ++ "."
      in
        ( { target | hitPoints = target.hitPoints - dmg }, [ msg ] )

    Heal ->
      let
        msg =
          user.name ++ " heals " ++ target.name ++ " for 30 hitpoints."
      in
        ( { target | hitPoints = target.hitPoints + 30 }, [ msg ] )

    _ ->
      Debug.crash "not a single target move"


selfReaction : Combatant -> Move -> ( Combatant, List String )
selfReaction user mv =
  case mv of
    Defend ->
      ( Combatant.increaseAP (Combatant.toDefendState user), [ user.name ++ " has started defending" ] )

    _ ->
      Debug.crash "not a self targetting move"


simulate : Command -> Simulation -> Maybe Simulation
simulate cmd initialSim =
  let
    with sim cmbt =
      case cmd of
        SingleTarget mv tid ->
          if Combatant.moveAvailable mv cmbt && existsAndAlive tid sim then
            let
              target =
                combatantByIdMustExist tid sim

              ( newTarget, logStuff ) =
                targetReaction cmbt mv (combatantByIdMustExist tid sim)

              nextSim =
                { sim
                  | combatants = Array.set tid newTarget sim.combatants
                  , combatLog = logStuff ++ sim.combatLog
                }
            in
              Just (dropActiveTurn nextSim)
          else
            Nothing

        SelfTarget mv ->
          if Combatant.moveAvailable mv cmbt then
            let
              ( newUser, logStuff ) =
                selfReaction cmbt mv

              nextSim =
                { sim
                  | combatants = Array.set cmbt.id newUser sim.combatants
                  , combatLog = logStuff ++ sim.combatLog
                }
            in
              Just (dropActiveTurn nextSim)
          else
            Nothing
  in
    case activeCmbt initialSim of
      Just cmbt ->
        let
          sim =
            modifyById Combatant.toDefaultState cmbt.id initialSim

          nextCmbt =
            case activeCmbt sim of
              Just c ->
                c

              Nothing ->
                Debug.crash "should never happen"
        in
          with sim nextCmbt

      Nothing ->
        Nothing
