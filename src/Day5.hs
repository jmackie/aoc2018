{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Day5
    ( printAnswer
    )
where

import Prelude

import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.IO as Text (readFile)
import qualified Paths_aoc2018 as Paths

import Data.Text (Text)


getQuestionInput :: IO Text
getQuestionInput = Text.readFile =<< Paths.getDataFileName "data/day5.txt"


printAnswer :: IO ()
printAnswer = do
    input <- Text.strip <$> getQuestionInput
    --       ^^^^^^^^^^^^ drop trailing newline !!

    putStr "\tPart 1: "
    print (partOne input)

    putStr "\tPart 2: "
    print (partTwo input)


partOne :: Text -> Int
partOne = Text.length . unUnit . foldMap singletonUnit . Text.unpack


partTwo :: Text -> Int
partTwo input = minimum
    (Text.length . unUnit . reactionWithout <$> ['a' .. 'z'])
  where
    reactionWithout :: Char -> Unit
    reactionWithout c =
        foldMap singletonUnit . Text.unpack . Text.filter (notLetter c) $ input


newtype Unit = Unit { unUnit :: Text } deriving newtype (Eq, Ord)

instance Semigroup Unit where (<>) = reactUnits

instance Monoid Unit where mempty = Unit mempty


singletonUnit :: Char -> Unit
singletonUnit = Unit . Text.singleton


reactUnits :: Unit -> Unit -> Unit
reactUnits (Unit lhs) (Unit rhs) = case unsnoc lhs of
    Nothing        -> Unit rhs
    Just (lhs', a) -> case uncons rhs of
        Nothing -> Unit lhs
        Just (b, rhs') | willReact a b -> Unit lhs' <> Unit rhs'
                       | otherwise     -> Unit (lhs <> rhs)
  where
    willReact :: Char -> Char -> Bool
    willReact = oppositeCase


uncons :: Text -> Maybe (Char, Text)
uncons "" = Nothing
uncons t  = Just (Text.head t, Text.tail t)


unsnoc :: Text -> Maybe (Text, Char)
unsnoc "" = Nothing
unsnoc t  = Just (Text.init t, Text.last t)


oppositeCase :: Char -> Char -> Bool
oppositeCase a b = abs (Char.ord b - Char.ord a) == 32


notLetter :: Char -> Char -> Bool
notLetter a b = (a /= b) && (Char.ord a /= Char.ord b + 32)
