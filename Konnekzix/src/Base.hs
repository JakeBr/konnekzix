{-# LANGUAGE TupleSections #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Konnekzix.Base
-- Copyright   :  (c) Jakob Brünker, 2013
-- License     :  BSD3
--
-- Maintainer  :  jake.bruenker@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- The types and basic functionality
--
--------------------------------------------------------------------------------

module Base
  ( Board()
  , Stone
  , Color
  , Coords
  , Row
  , Size
  , empty
  , placeStone
  , getStone
  , getRow
  ) where

import Data.Array
import Data.Maybe

-- | 'Board' is the type for storing the current data of a game. It contains
-- an array, which contains an element for every intersection on the board.
-- Each element may contain a stone and contains some information to see if
-- the stone is part of a row of multiple stones, horizontally, vertically,
-- or diagonally. To construct a board, use 'empty'.
newtype Board = Board (Array Coords (Maybe Stone, Array Direction Row))

-- | On each intersection of a board, there may be a Stone.
newtype Stone = Stone Color deriving Eq

-- | The directions of the 'Row's
data Direction = West
               | North
               | NorthWest
               | NorthEast
               deriving (Eq, Ord, Enum, Ix)

-- | Each player and their stones have a 'Color'.
data Color = Black
           | White
           deriving Eq

-- | 'Coords' are used to access intersections on a board.
type Coords = (Int,Int)

-- | A 'Row' value will tell you how many stones of the same color as this
-- one are placed in front of it. If there is no Stone, it will tell you
-- how many empty intersections are in front of it. (\"in front\" actually
-- means either above it, to the left, to the top-left or to the top-right.)
type Row = Int

-- | Each board has a 'Size'. A board has <size>^2 intersections.
type Size = Int

-- | The 'empty' function lets you construct a 'Board'. You need to give it
-- size which is greater than five.
empty :: Size -> Either String Board
empty s = if   s < 6 then Left "The size must be greater than five!"
          else Right . Board . fmap ((Nothing,) . listArray (West,NorthEast))
            . listArray ((1,1),(s,s)) $
            [ [x,y,min x y, min (s - x + 1) y] | x <- [1..s], y <- [1..s] ]

-- 'isOutOfBoundsOf' checks, whether the given Coordinates are valid for
-- a given Board
isOutOfBoundsOf :: Coords -> Board -> Bool
isOutOfBoundsOf coords (Board arr) = not $ inRange (bounds arr) coords

-- | With 'placeStone', you can place a stone on a board, who'd a thunk? If
-- someone has won because of that stone, you'll get a boolean truth in
-- addition. But beware, if you try to place a stone on top of another one
-- or even outside of the board, it will give you a String. It will also
-- update the 'Row's.
placeStone :: Board -> Stone -> Coords -> Either String (Bool, Board)
placeStone board@(Board arr) stone coords
  |  coords `isOutOfBoundsOf` board =
      Left "Coordinates are out of Bounds!"
  | isJust . fst $ arr ! coords =
      Left "Stones can only be placed on empty intersections"
  | otherwise =
      Right . fixRows coords . replace stone coords $ board

-- 'replace' let's you replace a stone without error handling. (the program
-- won't crash. If something is invalid, it will just return the original
-- 'Board'.) It will also not fix the 'Row's.
replace :: Stone -> Coords -> Board -> Board
replace stone coords board@(Board arr) =
  if   coords `isOutOfBoundsOf` board
  then board
  else Board $ arr // [(coords,(Just stone, snd old))]
    where old = arr ! coords

-- 'fixRows' will fix every 'Row' of a given 'Board'.
fixRows :: Coords -> Board -> (Bool, Board)
fixRows coords board =
  (any fst $ concat fixes, foldr1 (.) (map (flip (//>) . map snd) fixes) board)
  where fixes = map (fix board coords ) $
          zip3
            dirs
            (map prevStoneIn dirs)
            (map prevRowsIn dirs)
        dirs = [West .. NorthEast]
        prevStoneIn dir = getStone board $ goTo coords dir 1
        prevRowsIn dir  = getRow board (goTo coords dir 1) dir

-- 'fix' fixes a single continuous 'Row'. It is basically a helper
-- function for 'fixRows'.
fix :: Board -> Coords -> (Direction, Maybe Stone, Row) ->
  [(Bool, (Coords, Direction, Row))]
fix board coords (dir, prevStone, prevRow)
  | coords `isOutOfBoundsOf` board = []
  | isFine                         = []
  | prevStone /= thisStone         =
      (False, (coords, dir, 1)) : fix board newCoords (dir, thisStone, 1)
  | otherwise                      =
      (prevRow >= 5 && isJust thisStone, (coords, dir, prevRow + 1)) :
        fix board newCoords (dir, thisStone, prevRow + 1)
  where isFine = (prevStone == thisStone && prevRow == thisRow - 1) ||
                 (prevStone /= thisStone && thisRow == 1)
        thisRow   = getRow   board coords dir
        thisStone = getStone board coords
        newCoords = goTo coords dir (-1)

-- 'goTo' will give you coordinates for going a certain number of
-- intersections in a certain direction from the initial coordinates.
goTo :: Coords -> Direction -> Int -> Coords
goTo (x,y) dir amount = case dir of
  West      -> (x - amount, y         )
  North     -> (x         , y - amount)
  NorthWest -> (x - amount, y - amount)
  NorthEast -> (x + amount, y - amount)

-- '//>' is a variation of 'Data.Array.//' for specific usage with 'Row's.
(//>) :: Board -> [(Coords, Direction, Row)] -> Board
(//>) (Board arr) news = Board $ arr // map rephrase news
  where rephrase (coords,dir,row) =
          let old = arr ! coords
          in  (coords, (fst old, snd old // [(dir,row)]))

-- | 'getRow' returns the Row in a certain direction at given coordinates
getRow :: Board -> Coords -> Direction -> Row
getRow (Board arr) coords dir = snd (arr ! coords) ! dir

-- | 'getStone' looks at a intersection and returns Just the Stone or
-- Nothing, respectively.
getStone :: Board -> Coords -> Maybe Stone
getStone (Board arr) coords = fst (arr ! coords)
