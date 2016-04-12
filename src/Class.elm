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


module Class (Class(..), strength, speed, defense, moveList) where

import Move exposing (Move(..))


type Class
  = Warrior
  | Thief
  | Cleric


strength class =
  case class of
    Warrior ->
      1.0

    Thief ->
      1.2

    Cleric ->
      0.8


speed class =
  case class of
    Warrior ->
      8

    Thief ->
      11

    Cleric ->
      7


defense class =
  case class of
    Warrior ->
      1.0

    Thief ->
      1.4

    Cleric ->
      1.2


warriorMoveList =
  [ Attack, Defend ]


thiefMoveList =
  [ Attack, Defend ]


clericMoveList =
  [ Attack, Defend, Heal ]


moveList class =
  case class of
    Warrior ->
      warriorMoveList

    Thief ->
      thiefMoveList

    Cleric ->
      clericMoveList
