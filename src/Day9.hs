{-# LANGUAGE OverloadedLists #-}
module Day9
    ( printAnswer
    )
where

import Prelude

import qualified Data.List as List
import qualified Data.List.PointedList.Circular as PointedList
import qualified Data.Map as Map

import Control.Monad (foldM)
import Data.List.PointedList.Circular (PointedList)
import Data.Map (Map)
import Data.Ord (comparing)


printAnswer :: IO ()
printAnswer = do
    putStr "\tPart 1: "
    print (play 416 71975)
    -- 439341

    putStr "\tPart 2: "
    print (play 416 $ 71975 * 100)
    -- 3566801385


type Marble = Int
type Player = Int
type Score  = Int


play :: Int -> Int -> Maybe Int
play nplayers nmarbles = winningScore . fst <$> foldM
    playRound
    (Map.fromList $ zip players (repeat 0), PointedList.singleton 0)
    (zip (cycle players) marbles)
  where
    players :: [Player]
    players = [1 .. nplayers]

    marbles :: [Marble]
    marbles = [1 .. nmarbles]

    playRound
        :: (Map Player Score, PointedList Marble)
        -> (Player, Marble)
        -> Maybe (Map Player Score, PointedList Marble)
    playRound (scores, game) (player, marble)
        | marble `isMultipleOf` 23
        = let game'   = PointedList.moveN (-7) game
              marble' = PointedList._focus game'
          in  sequence
                  ( Map.insertWith (+) player (marble + marble') scores
                  , PointedList.deleteRight game'
                  )
        | otherwise
        = Just (scores, PointedList.insertRight marble (PointedList.next game))

    winningScore :: Map Player Score -> Score
    winningScore = snd . List.maximumBy (comparing snd) . Map.toList


isMultipleOf :: Integral a => a -> a -> Bool
isMultipleOf x y = x `mod` y == 0
