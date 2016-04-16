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


module UserInterface (..) where

import Id exposing (Id)
import Move exposing (Move)
import Simulation exposing (Simulation)
import Player exposing (Player(..))
import Combatant exposing (Combatant)
import Command exposing (Command(..), CommandType(..))
import AI.AlphaBeta as AlphaBeta
import Html exposing (Html, div, button, text, span)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Signal exposing (Address)
import List
import Array
import Debug
import Maybe
import String


type alias Model =
  { sim : Simulation
  , mov : Maybe Move
  }


type Action
  = SelectMove Move
  | SelectTarget Id
  | CancelSelection


update : Action -> Model -> Model
update action model =
  case action of
    SelectMove mv ->
      case Command.typeOfMove mv of
        SingleTargetType ->
          { model | mov = Just mv }

        SelfTargetType ->
          case Simulation.simulate (SelfTarget mv) model.sim of
            Just sim ->
              { model | sim = Simulation.clockTickUntilTurn (aiIfNecessary sim), mov = Nothing }

            Nothing ->
              Debug.crash "this should not be possible"

    SelectTarget id ->
      case model.mov of
        Just mov ->
          case Simulation.simulate (SingleTarget mov id) model.sim of
            Just sim ->
              { model | sim = Simulation.clockTickUntilTurn (aiIfNecessary sim), mov = Nothing }

            Nothing ->
              Debug.crash "this should not be possible"

        Nothing ->
          Debug.crash "this should not be possible"

    CancelSelection ->
      { model | mov = Nothing }


aiIfNecessary : Simulation -> Simulation
aiIfNecessary sim =
  if Simulation.gameOver sim then
    sim
  else
    case Simulation.whosTurn (Simulation.clockTickUntilTurn sim) of
      Just AI ->
        aiIfNecessary (AlphaBeta.playAI sim)

      Just User ->
        sim

      Nothing ->
        Debug.crash "this should not be possible"


view : Address Action -> Model -> Html
view addr model =
  div
    [ class "game" ]
    [ div
        [ class "main" ]
        [ (div [ class "ai-party" ] [ viewParty addr AI model ])
        , (div [ class "user-party" ] [ viewParty addr User model ])
        , (viewCombatLog model.sim.combatLog)
        ]
    , viewCtBar model
    ]


viewCombatLog : List String -> Html
viewCombatLog log =
  div
    [ class "combat-log" ]
    (log |> List.take 10 |> List.indexedMap viewCombatLogLine)


viewCombatLogLine : Int -> String -> Html
viewCombatLogLine idx line =
  let
    op =
      toString (1.0 - (toFloat idx * 8.0e-2))
  in
    div
      [ class "combat-log-line"
      , style [ ( "opacity", op ) ]
      ]
      [ text line ]


viewCtBar : Model -> Html
viewCtBar model =
  let
    order =
      Simulation.turnOrderList model.sim
  in
    div
      [ class "ct-bar" ]
      ((text "Turn Order") :: (order |> List.indexedMap viewCtBarUnit))


viewCtBarUnit : Int -> Combatant -> Html
viewCtBarUnit n cmbt =
  div
    [ class "ct-bar-unit" ]
    [ span [ class "ct-bar-unit-num" ] [ text (toString (n + 1)) ]
    , text cmbt.name
    ]


viewParty : Address Action -> Player -> Model -> Html
viewParty addr player model =
  div
    [ class "party" ]
    (Simulation.party player model.sim
      |> Array.map (viewCombatant addr player model)
      |> Array.toList
    )


tooltip : String -> Html
tooltip tip =
  div
    [ class "tooltip" ]
    [ text tip ]


viewCombatant : Address Action -> Player -> Model -> Combatant -> Html
viewCombatant addr player model cmbt =
  div
    [ if Combatant.alive cmbt then
        class "combatant combatant-alive"
      else
        class "combatant combatant-dead"
    ]
    [ div
        [ class "combatant-status-bar" ]
        [ (span
            [ class "combatant-name tooltip-container" ]
            [ text cmbt.name
            , tooltip "The unit's name"
            ]
          )
        , (span
            [ class "combatant-class tooltip-container" ]
            [ text (toString cmbt.class)
            , tooltip "The unit's class"
            ]
          )
        , (div
            [ class "combatant-hp tooltip-container" ]
            [ span
                [ class "combatant-hp-label" ]
                [ text "HP" ]
            , text (toString (ceiling cmbt.hitPoints))
            , tooltip "Health"
            ]
          )
        , viewCombatantAP cmbt
        , (div
            [ class "combatant-ct tooltip-container" ]
            [ span
                [ class "combatant-ct-label" ]
                [ text "CT" ]
            , text (toString cmbt.chargeTime)
            , tooltip "Charge time, unit takes a turn when at least 100"
            ]
          )
        ]
    , if Combatant.alive cmbt then
        case ( Simulation.doIHaveActiveTurn cmbt.id model.sim, player, model.mov ) of
          ( True, User, Just mv ) ->
            viewTargets addr player model

          ( True, User, Nothing ) ->
            viewMoves addr cmbt

          _ ->
            text ""
      else
        text ""
    ]


viewCombatantAP : Combatant -> Html
viewCombatantAP cmbt =
  div
    [ class "combatant-ap tooltip-container" ]
    [ span [ class "combatant-ap-label" ] [ text "AP" ]
    , span [ class "combatant-ap-filled" ] [ text (String.repeat cmbt.actionPoints "•") ]
    , span [ class "combatant-ap-empty" ] [ text (String.repeat (5 - cmbt.actionPoints) "•") ]
    , tooltip ("This unit has " ++ toString cmbt.actionPoints ++ " AP to spend on moves")
    ]


viewMoves : Address Action -> Combatant -> Html
viewMoves addr cmbt =
  div
    [ class "combatant-move-list" ]
    (Combatant.moveList cmbt |> List.map (viewMove addr cmbt))


viewMove : Address Action -> Combatant -> Move -> Html
viewMove addr unit mv =
  button
    (if unit.actionPoints >= Move.cost mv then
      [ class "combatant-move tooltip-container"
      , onClick addr (SelectMove mv)
      ]
     else
      [ class "combatant-move tooltip-container combatant-move-unusable" ]
    )
    [ text (toString mv ++ " " ++ String.repeat (Move.cost mv) "•")
    , tooltip (Move.tooltip mv)
    ]


viewTargets : Address Action -> Player -> Model -> Html
viewTargets addr player model =
  div
    [ class "combatant-target-list" ]
    [ div
        [ class "combatant-target-party" ]
        (Simulation.combatants model.sim
          |> Array.filter (Combatant.foesOf player)
          |> Array.map (viewTarget addr)
          |> Array.toList
        )
    , div
        [ class "combatant-target-party" ]
        (Simulation.combatants model.sim
          |> Array.filter (Combatant.friendsOf player)
          |> Array.map (viewTarget addr)
          |> Array.toList
        )
    , button
        [ class "combatant-target-cancel"
        , onClick addr CancelSelection
        ]
        [ text "Cancel" ]
    ]


viewTarget : Address Action -> Combatant -> Html
viewTarget addr cmbt =
  let
    attributes =
      if Combatant.alive cmbt then
        [ class "combatant-target combatant-target-alive"
        , onClick addr (SelectTarget cmbt.id)
        ]
      else
        [ class "combatant-target combatant-target-dead" ]
  in
    button
      attributes
      [ text cmbt.name ]
