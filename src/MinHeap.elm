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


module MinHeap (MinHeap, empty, insert, findMin, deleteMin) where

import Debug


type alias BranchRec a =
  { count : Int
  , left : MinHeap a
  , right : MinHeap a
  , time : Int
  , item : a
  }


type MinHeap a
  = Empty
  | Branch (BranchRec a)


empty : MinHeap a
empty =
  Empty


insert : Int -> a -> MinHeap a -> MinHeap a
insert t x heap =
  let
    recur subHeap =
      case subHeap of
        Empty ->
          Branch { count = 0, left = Empty, right = Empty, time = t, item = x }

        Branch branch ->
          if count branch.left < count branch.right then
            mkBranch <| trySwapLeft { branch | left = recur branch.left }
          else
            mkBranch <| trySwapRight { branch | right = recur branch.right }
  in
    recur heap


findMin : MinHeap a -> Maybe ( Int, a )
findMin heap =
  case heap of
    Empty ->
      Nothing

    Branch branch ->
      Just ( branch.time, branch.item )


deleteMin : MinHeap a -> MinHeap a
deleteMin heap =
  stealLastElement heap |> heapDown


mkBranch : BranchRec a -> MinHeap a
mkBranch branch =
  Branch { branch | count = count branch.left + count branch.right }


count : MinHeap a -> Int
count heap =
  case heap of
    Empty ->
      0

    Branch branch ->
      branch.count


stealLastElement : MinHeap a -> MinHeap a
stealLastElement heap =
  let
    recur subHeap =
      case subHeap of
        Empty ->
          Debug.crash "should never be empty"

        Branch branch ->
          if branch.left == Empty && branch.right == Empty then
            ( branch.time, branch.item, Empty )
          else if count branch.left > count branch.right then
            let
              ( t, x, h ) =
                recur branch.left
            in
              ( t, x, mkBranch { branch | left = h } )
          else
            let
              ( t, x, h ) =
                recur branch.right
            in
              ( t, x, mkBranch { branch | right = h } )
  in
    case heap of
      Empty ->
        heap

      Branch _ ->
        case recur heap of
          ( t, x, Branch h ) ->
            mkBranch { h | time = t, item = x }

          _ ->
            Debug.crash "should not be possible"


heapDown : MinHeap a -> MinHeap a
heapDown heap =
  case heap of
    Empty ->
      heap

    Branch branch ->
      if count branch.left > count branch.right then
        mkBranch <| trySwapLeftDown branch
      else
        mkBranch <| trySwapRightDown branch


trySwapLeftDown : BranchRec a -> BranchRec a
trySwapLeftDown branch =
  case branch.left of
    Empty ->
      branch

    Branch left ->
      if left.time < branch.time then
        { branch
          | item = left.item
          , time = left.time
          , left = heapDown (mkBranch { left | item = branch.item, time = branch.time })
        }
      else
        branch


trySwapRightDown : BranchRec a -> BranchRec a
trySwapRightDown branch =
  case branch.right of
    Empty ->
      branch

    Branch right ->
      if right.time < branch.time then
        { branch
          | item = right.item
          , time = right.time
          , right = heapDown (mkBranch { right | item = branch.item, time = branch.time })
        }
      else
        branch


trySwapLeft : BranchRec a -> BranchRec a
trySwapLeft branch =
  case branch.left of
    Empty ->
      Debug.crash "should only be called on branches"

    Branch left ->
      if left.time < branch.time then
        { branch
          | item = left.item
          , time = left.time
          , left = mkBranch { left | item = branch.item, time = branch.time }
        }
      else
        branch


trySwapRight : BranchRec a -> BranchRec a
trySwapRight branch =
  case branch.right of
    Empty ->
      Debug.crash "should only be called on branches"

    Branch right ->
      if right.time < branch.time then
        { branch
          | item = right.item
          , time = right.time
          , right = mkBranch { right | item = branch.item, time = branch.time }
        }
      else
        branch
