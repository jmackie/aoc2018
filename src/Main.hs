module Main (main) where

import Prelude

import Data.Foldable (for_)

-- Answers:
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8

import qualified Day10


main :: IO ()
main = for_ (drop 8 answers) $ \(dayName, printAnswer) -> do
    putStrLn dayName
    printAnswer


answers :: [(String, IO ())]
answers =
    [ ("Day1" , Day1.printAnswer)
    , ("Day2" , Day2.printAnswer)
    , ("Day3" , Day3.printAnswer)
    , ("Day4" , Day4.printAnswer)
    , ("Day5" , Day5.printAnswer)
    , ("Day6" , Day6.printAnswer)
    , ("Day7" , Day7.printAnswer)
    , ("Day8" , Day8.printAnswer)
    , ("Day10", Day10.printAnswer)
    ]
