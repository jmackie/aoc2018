{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Day1 (printAnswer) where

import Prelude

import qualified Control.Exception as Exception
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text (readFile)
import qualified Paths_aoc2018 as Paths

import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Text (Text)
import Text.Read (readMaybe)


getQuestionInput :: IO Text
getQuestionInput = Text.readFile =<< Paths.getDataFileName "data/day1.txt"


printAnswer :: IO ()
printAnswer = do
    input   <- getQuestionInput
    changes <- parseChanges input & either Exception.throwIO pure

    -- Part 1:
    -- Apply all changes to an initialFrequency of 0
    putStr "\tPart 1: "
    print (applyChanges zeroFrequency changes)

    -- Part 2:
    -- Following from part 1, find the first duplicate frequency.
    -- Changes are cycled.
    putStr "\tPart 2: "
    print (findFirstDuplicate . scanChanges zeroFrequency $ cycle changes)


newtype Frequency = Frequency Integer
    deriving newtype (Show, Read, Num, Eq, Ord)


zeroFrequency :: Frequency
zeroFrequency = Frequency 0


newtype Change = Change { applyChange :: Frequency -> Frequency }


applyChanges :: Frequency -> [Change] -> Frequency
applyChanges = foldl' go
  where
    go :: Frequency -> Change -> Frequency
    go frequency change = applyChange change frequency


scanChanges :: Frequency -> [Change] -> [Frequency]
scanChanges = scanl go
  where
    go :: Frequency -> Change -> Frequency
    go frequency change = applyChange change frequency


findFirstDuplicate :: forall a . Ord a => [a] -> Maybe a
findFirstDuplicate = go Set.empty
  where
    go :: Set.Set a -> [a] -> Maybe a
    go _ [] = Nothing
    go set (a : as) | Set.member a set = Just a
                    | otherwise        = go (Set.insert a set) as


-- PARSING


parseChanges :: Text -> Either ParseError [Change]
parseChanges = sequence . zipWith parseLine [1 ..] . Text.lines
  where
    parseLine :: Integer -> Text -> Either ParseError Change
    parseLine lineNumber line = first (ParseError lineNumber line) $ do
        (op, integer) <- Text.uncons line ? EmptyLine
        frequency     <- parseFrequency integer ? BadInteger integer
        operation     <- operationFromChar op ? BadOperation op
        case operation of
            Add      -> pure $ Change (+ frequency)
            Subtract -> pure $ Change (subtract frequency)

    parseFrequency :: Text -> Maybe Frequency
    parseFrequency = readMaybe . Text.unpack

    (?) :: Maybe a -> ParseError' -> Either ParseError' a
    (?) mba err = maybe (Left err) Right mba


data Operation = Add | Subtract


operationFromChar :: Char -> Maybe Operation
operationFromChar '+' = Just Add
operationFromChar '-' = Just Subtract
operationFromChar _   = Nothing


data ParseError = ParseError Integer Text ParseError'
    deriving Exception.Exception


instance Show ParseError where
    show (ParseError lineNumber _line reason) =
        "parsing error: line " <> show lineNumber <> show reason


data ParseError'
    = EmptyLine
    | BadOperation Char
    | BadInteger Text


instance Show ParseError' where
    show EmptyLine           = "empty line"
    show (BadOperation char) = "unknown operator " <> [char]
    show (BadInteger text)   = "expecting an integer, got " <> Text.unpack text
