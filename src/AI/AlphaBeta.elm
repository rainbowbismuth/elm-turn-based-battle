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


module AI.AlphaBeta (..) where

import Simulation exposing (Simulation)
import Combatant exposing (Combatant)
import Player exposing (Player(..))
import Command exposing (Command(..), CommandType(..))
import Move exposing (Move)
import Array
import Debug


scoreCombatant : Combatant -> Float
scoreCombatant cmbt =
  let
    bonus =
      if Combatant.alive cmbt then
        50
      else
        0

    score =
      (max 0 cmbt.hitPoints) + bonus
  in
    if cmbt.player == AI then
      score
    else
      -score


score : Simulation -> Float
score sim =
  Array.foldr (\x y -> scoreCombatant x + y) 0 (Simulation.combatants sim)


targetsForMove : Simulation -> Move -> List Command
targetsForMove sim mv =
  case Command.typeOfMove mv of
    SingleTargetType ->
      sim.combatants
        |> Array.toList
        |> List.filterMap
            (\target ->
              if Combatant.alive target then
                Just (SingleTarget mv target.id)
              else
                Nothing
            )

    SelfTargetType ->
      [ SelfTarget mv ]


availableMoves : Simulation -> List Command
availableMoves sim =
  Combatant.moveList (Simulation.activeCmbtMustExist sim)
    |> List.concatMap (targetsForMove sim)


inf : Float
inf =
  256000.0


evaluatePosition : Simulation -> Int -> Float
evaluatePosition sim depth =
  alphabeta sim depth (-inf) inf


alphabeta : Simulation -> Int -> Float -> Float -> Float
alphabeta sim depth a b =
  if depth == 0 || Simulation.gameOver sim then
    score sim
  else
    case Simulation.whosTurn sim of
      Just AI ->
        alphabetaMaximizing (availableMoves sim) sim depth a b (-inf)

      Just User ->
        alphabetaMinimizing (availableMoves sim) sim depth a b (inf)

      Nothing ->
        alphabeta (Simulation.clockTick sim) depth a b


alphabetaMaximizing moves sim depth a b v =
  case moves of
    m :: ms ->
      case Simulation.simulate m sim of
        Just nextSim ->
          let
            nextV =
              max v (alphabeta nextSim (depth - 1) a b)

            nextA =
              max a nextV
          in
            if b < nextA then
              nextV
            else
              alphabetaMaximizing ms sim depth nextA b nextV

        Nothing ->
          alphabetaMaximizing ms sim depth a b v

    [] ->
      v


alphabetaMinimizing moves sim depth a b v =
  case moves of
    m :: ms ->
      case Simulation.simulate m sim of
        Just nextSim ->
          let
            nextV =
              min v (alphabeta nextSim (depth - 1) a b)

            nextB =
              min b nextV
          in
            if nextB < a then
              nextV
            else
              alphabetaMinimizing ms sim depth a nextB nextV

        Nothing ->
          alphabetaMinimizing ms sim depth a b v

    [] ->
      v


playAI : Simulation -> Simulation
playAI sim =
  let
    explore cmd =
      let
        nextSim =
          case Simulation.simulate cmd sim of
            Just sim ->
              Simulation.clockTickUntilTurn sim

            Nothing ->
              Debug.crash "this shouldn't be happening"

        score =
          evaluatePosition nextSim 4
      in
        ( cmd, nextSim, score )

    head =
      availableMoves sim
        |> List.map explore
        |> List.sortBy (\( c, s, f ) -> -f)
        |> Debug.log "AI move list"
        |> List.head
  in
    case head of
      Just ( _, sim, _ ) ->
        sim

      Nothing ->
        Debug.crash "this shouldn't happen"
