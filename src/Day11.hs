{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Day11
    ( printAnswer
    )
where

import Prelude

import qualified Data.List as List
import qualified Data.Vector as Vector

import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Vector (Vector)


-- Puzzle input
mySerialNum :: Int
mySerialNum = 7347


printAnswer :: IO ()
printAnswer = do
    -- Tests
    guard (powerLevel 57 (122, 79) == -5)
    guard (powerLevel 39 (217, 196) == 0)
    guard (powerLevel 71 (101, 153) == 4)

    putStr "\tPart 1: "
    print partOne
    -- (243, 17)

    putStr "\tPart 2: "
    --print partTwo
    putStrLn "skipped (v slow)"


partOne :: (Int, Int)
partOne = snd $ solution mySerialNum 3


_partTwo :: (Int, Int, Int)
_partTwo =
    let (power, (x, y)) = solution mySerialNum 1
    in  go (x, y, 1) power [2 .. 300]
  where
    go :: (Int, Int, Int) -> Power -> [Int] -> (Int, Int, Int)
    go result _ [] = result
    go (x, y, size) power (size' : rest) =
        let (power', (x', y')) = solution mySerialNum size'
        in  if power' > power
                then go (x', y', size') power' rest
                else go (x, y, size) power rest


solution :: Int -> Int -> (Power, (Int, Int))
solution serialNum size = swap <$> List.maximumBy
    (comparing fst)
    [ (totalPower (i, j), (i, j))
    | i <- [1 .. 300 - pred size]
    , j <- [1 .. 300 - pred size]
    ]
  where
    summedRack :: Grid Power
    summedRack = summedAreaGrid
        $ mkGrid 300 300 (\(i, j) -> powerLevel serialNum (j + 1, i + 1))

    totalPower :: (Int, Int) -> Power
    totalPower (i, j) =
        fromMaybe undefined (area (i - 1, j - 1) size size summedRack)


type Power = Int


powerLevel :: Int -> (Int, Int) -> Power
powerLevel serialNum (x, y) =
    let rackId = x + 10
    in  maybe (negate 5)
              (subtract 5)
              (hundredsDigit $ (rackId * y + serialNum) * rackId)


-- | Get area from a summed area grid.
area :: (Int, Int) -> Int -> Int -> Grid Int -> Maybe Int
area (i, j) nrow ncol grid
    | i == 0 || j == 0 = gridCell (i, j) grid
    | otherwise = do
        tl <- gridCell (i - 1, j - 1) grid
        tr <- gridCell (i - 1, j + pred ncol) grid
        bl <- gridCell (i + pred nrow, j - 1) grid
        br <- gridCell (i + pred nrow, j + pred ncol) grid
        pure (tl + (br - tr - bl))


hundredsDigit :: Int -> Maybe Int
hundredsDigit i = case digits i of
    (_ : _ : d : _) -> Just d
    _               -> Nothing


-- |
-- @digits 123 == [3,2,1]@
digits :: Integral a => a -> [a]
digits 0 = []
digits a = a `mod` 10 : digits (a `div` 10)


data Grid a = Grid
    { _nrow  :: !Int
    , _ncol  :: !Int
    , _items :: Vector a
    }


mkGrid :: Int -> Int -> ((Int, Int) -> a) -> Grid a
mkGrid nrow ncol f = Grid nrow ncol $ Vector.fromList
    [ f (i, j) | i <- [0 .. pred nrow], j <- [0 .. pred ncol] ]


gridCell :: (Int, Int) -> Grid a -> Maybe a
gridCell (i, j) (Grid nrow ncol items)
    | not ok    = Nothing
    | otherwise = Just (gridCell' (i, j) (Grid nrow ncol items))
  where
    ok :: Bool
    ok = i >= 0 && j >= 0 && i < nrow && j < ncol


gridCell' :: (Int, Int) -> Grid a -> a
gridCell' (i, j) (Grid _ ncol items) = Vector.unsafeIndex items (j + i * ncol)


gridRows :: Grid a -> [Vector a]
gridRows (Grid nrow ncol items) =
    [ Vector.unsafeSlice start ncol items
    | start <- [0, ncol .. nrow * pred ncol]
    ]


-- | https://en.wikipedia.org/wiki/Summed-area_table
summedAreaGrid :: forall a . Num a => Grid a -> Grid a
summedAreaGrid grid = case gridRows grid of
    [] -> grid
    r : rs ->
        let summed = Vector.concat $ List.scanl'
                (\r' -> Vector.zipWith (+) r' . scanSum)
                (scanSum r)
                rs
        in  grid { _items = summed }
  where
    scanSum :: Vector a -> Vector a
    scanSum [] = []
    scanSum as = Vector.scanl1' (+) as


swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)
