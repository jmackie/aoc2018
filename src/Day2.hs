{-# LANGUAGE ScopedTypeVariables #-}
module Day2 (printAnswer) where

import Prelude

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text (readFile)
import qualified Paths_aoc2018 as Paths

import Data.Foldable (foldl')
import Data.Text (Text)


getQuestionInput :: IO Text
getQuestionInput = Text.readFile =<< Paths.getDataFileName "data/day2.txt"


printAnswer :: IO ()
printAnswer = do
    input <- getQuestionInput

    -- Part 1:
    putStr "\tPart 1: "
    print (checksum input)

    -- Part 1:
    putStr "\tPart 2: "
    print (uncurry commonChars <$> findPrototypeBoxIds input)


type Count = Int


checksum :: Text -> Int
checksum = go 0 0 . Text.lines
  where
    go :: Count -> Count -> [Text] -> Int
    go duplicates triplicates [] = duplicates * triplicates
    go duplicates triplicates (line : rest) =
        case (findDuplicate line, findTriplicate line) of
            (Just _ , Just _ ) -> go (duplicates + 1) (triplicates + 1) rest
            (Just _ , Nothing) -> go (duplicates + 1) triplicates rest
            (Nothing, Just _ ) -> go duplicates (triplicates + 1) rest
            (Nothing, Nothing) -> go duplicates triplicates rest

    findDuplicate :: Text -> Maybe Char
    findDuplicate =
        fmap fst . List.find ((== 2) . snd) . countOccurrences . Text.unpack

    findTriplicate :: Text -> Maybe Char
    findTriplicate =
        fmap fst . List.find ((== 3) . snd) . countOccurrences . Text.unpack


findPrototypeBoxIds :: Text -> Maybe (Text, Text)
findPrototypeBoxIds = go . Text.lines
  where
    go :: [Text] -> Maybe (Text, Text)
    go []            = Nothing
    go (line : rest) = case List.find (differsByOneChar line) rest of
        Nothing    -> go rest
        Just match -> Just (line, match)

    differsByOneChar :: Text -> Text -> Bool
    differsByOneChar a b =
        countTrue (zipWith (/=) (Text.unpack a) (Text.unpack b)) == 1


countOccurrences :: forall a . Ord a => [a] -> [(a, Count)]
countOccurrences =
    Map.toList . foldl' (\accum a -> Map.insertWith (+) a 1 accum) Map.empty


countTrue :: [Bool] -> Count
countTrue []       = 0
countTrue (b : bs) = if b then 1 + countTrue bs else countTrue bs


{-# ANN commonChars "HLint: ignore Use String" #-}
commonChars :: Text -> Text -> [Char]
commonChars a b =
    fst <$> filter (uncurry (==)) (zip (Text.unpack a) (Text.unpack b))
