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


main :: IO ()
main = for_ answers $ \(dayName, printAnswer) -> do
    putStrLn dayName
    printAnswer


answers :: [(String, IO ())]
answers =
    [ ("Day1", Day1.printAnswer)
    , ("Day2", Day2.printAnswer)
    , ("Day3", Day3.printAnswer)
    , ("Day4", Day4.printAnswer)
    , ("Day5", Day5.printAnswer)
    , ("Day6", Day6.printAnswer)
    , ("Day7", Day7.printAnswer)
    ]
