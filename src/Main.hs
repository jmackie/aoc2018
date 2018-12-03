module Main (main) where

import Prelude

import Data.Foldable (for_)

-- Answers:
import qualified Day1
import qualified Day2
import qualified Day3


main :: IO ()
main = for_ answers $ \(dayName, printAnswer) -> do
    putStrLn dayName
    printAnswer


answers :: [(String, IO ())]
answers =
    [ ("Day1", Day1.printAnswer)
    , ("Day2", Day2.printAnswer)
    , ("Day3", Day3.printAnswer)
    ]
