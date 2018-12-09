module Day8
    ( printAnswer
    )
where

import Prelude

import qualified Control.Monad.Scanner as Scanner
import qualified Data.Text as Text
import qualified Data.Text.IO as Text (readFile)
import qualified Paths_aoc2018 as Paths

import Control.Monad (replicateM)
import Control.Monad.Scanner (Scanner)
import Data.Function ((&))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Text.Read (readEither)


getQuestionInput :: IO Text
getQuestionInput = Text.readFile =<< Paths.getDataFileName "data/day8.txt"


printAnswer :: IO ()
printAnswer = do
    input <- getQuestionInput
    tree  <- parseInts input >>= parseTree & either error pure

    putStr "\tPart 1: "
    print (partOne tree)
    -- 41028

    putStr "\tPart 2: "
    print (partTwo tree)
    -- 20849


partOne :: Node Int -> Int
partOne (Node children meta) = sum meta + sum (partOne <$> children)


partTwo :: Node Int -> Int
partTwo (Node []       meta) = sum meta
partTwo (Node children meta) = sum . flip mapMaybe meta $ \i -> do
    child <- children !? pred i
    pure (partTwo child)


data Node a = Node [Node a] [a]


parseTree :: [Int] -> Either String (Node Int)
parseTree ints = case Scanner.run nodeScanner ints of
    Left  err        -> Left (show err)
    Right (tree, []) -> Right tree
    Right (_   , _ ) -> Left "leftover input!"
  where
    nodeScanner :: Scanner Int (Node Int)
    nodeScanner = do
        childCount <- Scanner.scan
        metaCount  <- Scanner.scan
        children   <- replicateM childCount nodeScanner
        meta       <- replicateM metaCount Scanner.scan
        pure (Node children meta)


parseInts :: Text -> Either String [Int]
parseInts = traverse (readEither . Text.unpack) . Text.words


infixl 9 !?
(!?) :: [a] -> Int -> Maybe a
(!?) as i | i < length as = Just (as !! i)
          | otherwise     = Nothing
