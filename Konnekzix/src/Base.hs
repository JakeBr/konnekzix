{-# LANGUAGE TupleSections #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Konnekzix.Base
-- Copyright   :  (c) Jake Brünker, 2013
-- License     :  BSD3
--
-- Maintainer  :  jake.bruenker@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- The types and basic functionality
--
--------------------------------------------------------------------------------

module Base where

import Data.Array
import Data.Maybe
import Data.List.Split
import Data.List

-- | 'Board' is the type for storing the current data of a game. It contains
-- an array, which contains an element for every intersection on the board.
-- Each element may contain a stone and contains some information to see if
-- the stone is part of a row of multiple stones, horizontally, vertically,
-- or diagonally. To construct a board, use 'empty'.
newtype Board = Board (Array Coords (Maybe Stone, Array Int Row))

-- | On each intersection of a board, there may be a Stone.
newtype Stone = Stone Color deriving Eq

-- | Each player and their stones have a 'Color'.
data Color = Black
           | White
           deriving Eq

-- | 'Coords' are used to access intersections on a board.
type Coords = (Int,Int)

-- | A 'Row' value will tell you how many stones of the same color as this
-- one are placed in front of it. If there is no Stone, it will tell you
-- how many empty intersections are in front of it. (\"In front\" actually
-- means either above it, to the left, to the top-left or to the top-right.)
type Row = Int

-- | Each board has a 'Size'. A board has <size>^2 intersections.
type Size = Int

-- | The 'empty' function lets you construct a 'Board'. You need to give it
-- size which is greater than six.
empty :: Size -> Either String Board
empty s = if   s < 6 then Left "The size must be greater than five!"
          else Right . Board . fmap ((Nothing,) . listArray (1,4))
            . listArray ((1,1),(s,s)) $
            [ [x,y,min x y, min (s - x + 1) y] | y <- [1..s], x <- [1..s] ]

-- | With 'placeStone', you can place a stone on a board, who'd a thunk? If
-- someone has won because of that stone, you'll get a boolean truth in
-- addition. But beware, if you try to place a stone on top of another one
-- or even outside of the board, it will give you a String. It will also
-- update the 'Row's.
placeStone :: Board -> Stone -> Coords -> Either String (Bool, Board)
placeStone (Board arr) stone coords@(x,y)
  | not $ inRange bs coords =
    Left "Coordinates are out of bounds!"
  | isJust . fst $ arr ! coords =
    Left "Stones can only be placed on empty intersections"
  | otherwise = Right
                (won, Board fixSucs)
    where (wonPred,getPredRows) =
            let rows = map getRow [1..4]
            in  (any id $ map fst rows,
                listArray (1,4) . map snd $ rows)
          bs = bounds arr
          won = wonPred || wonSucs
          getRow d = let predPos = arr ! cs (-1) d
                         row     = (snd predPos ! d) + 1
                     in  if   fst predPos /= Just stone
                         then (False, 1)
                         else (row >= 6,row)
          cs n d
            | d == 1    = (x + n, y    )
            | d == 2    = (x    , y + n)
            | d == 3    = (x + n, y + n)
            | otherwise = (x - n, y + n)
          (wonSucs,fixSucs) = let fixed = map (fix 2 .
                                          (\d -> (d,map (`cs` d) [1..])))
                                          [1..4]
                              in  (any id $ map fst fixed
                                  ,arr' // concatMap snd fixed)
          arr' = arr // [(coords,(Just stone,getPredRows))]
          fix :: Int -> (Int, [Coords]) ->
                 (Bool,[(Coords, (Maybe Stone, Array Int Row))])
          fix n (d, l@(xy:xys))
            | not $ inRange bs xy       = (False,[])
            | isNothing $ fst nextPos   = (False,fixNothings 1 l)
            | fst nextPos /= Just stone = (False,[])
            | otherwise                 = (n >= 5 || fst nextFix,
                                          snd nextFix ++
                                          [(xy,mapSnd (//[(d,n)]) nextPos)])
            where nextFix = fix (n + 1) (d,xys)
                  nextPos = arr ! xy
                  mapSnd f (p,q) = (p,f q)
                  fixNothings :: Int -> [Coords] ->
                                  [(Coords, (Maybe Stone, Array Int Row))]
                  fixNothings n' (xy':xy's)
                    | not $ inRange bs xy'  = []
                    | isJust $ fst nextPosN = []
                    | otherwise             = nextFixN ++
                                              [(xy, mapSnd (//[(d,n')])
                                              nextPosN)]
                    where nextPosN = arr ! xy'
                          nextFixN = fixNothings (n' + 1) xy's
                  fixNothings _ _ = []
          fix _ _ = (False,[])
-- Geeze, that's kind of a long function. Gonna fix that while refactoring
-- the code.

prettyDebug :: Board -> String
prettyDebug (Board arr) = firstLn ++ concatPieces (fmap (toPiece 1) arr) ++ "|"
  where firstLn      = concat (replicate maxX " _____") ++ "\n"
        toPiece n e  = let isNewLn  = n > maxX
                           endBar   = if isNewLn then "|"  else ""
                           newLn    = if isNewLn then "\n" else ""
                           sndLn    = fmap cutoff [d 3,0,d 2,0,d 4]
                           thrdLn   = cutoff (d 1) : "____"
                           cutoff x
                             | x == 0    = ' '
                             | otherwise = last . show $ x
                           d a      = snd e ! a
                       in  [ "|" ++ stone e ++ endBar ++ newLn
                           , "|" ++ sndLn   ++ endBar ++ newLn
                           , "|" ++ thrdLn  ++ endBar ++ newLn]
        maxX         = snd . snd . bounds $ arr
        concatPieces a = let go ((l1,l2,l3):ls) = l1 ++ l2 ++ l3 ++ go ls
                             go [] = []
                         in  addNewLns . go $ zip3 (line a 1) (line a 2) (line a 3)
        line a n     = chunksOf (maxX * 6) $ concat (fmap (!! (n - 1)) (elems a))
        addNewLns s  = let go wasB (x:xs)
                             | wasB = if x == '|' then go True xs else x : go False xs
                             | otherwise = if x == '|' then x : go True xs
                                                       else x : go False xs
                           go _ [] = []
                       in  go False . intercalate "|\n" $ chunksOf (maxX * 6) s
        stone t      = case fst t of
                         Nothing            -> "Nothn"
                         Just (Stone Black) -> "Black"
                         Just (Stone White) -> "White"
