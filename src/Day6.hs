{-# LANGUAGE TupleSections #-}
module Day6
    ( printAnswer
    )
where

import Prelude

import qualified Data.Attoparsec.Text as Parse
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text.IO as Text (readFile)
import qualified Paths_aoc2018 as Paths

import Control.Monad (join)
import Data.Foldable (foldl')
import Data.Function (on, (&))
import Data.Map (Map)
import Data.Monoid (Sum(Sum, getSum))
import Data.Ord (comparing)
import Data.Text (Text)


getQuestionInput :: IO Text
getQuestionInput = Text.readFile =<< Paths.getDataFileName "data/day6.txt"


printAnswer :: IO ()
printAnswer = do
    input  <- getQuestionInput
    coords <- parseCoordinates input & either error pure

    putStr "\tPart 1: "
    print (partOne coords)
    -- Just 3969

    putStr "\tPart 2: "
    print (partTwo coords)
    -- 42123


partOne :: [Coordinate] -> Maybe Int
partOne coords =
    fmap getSum
        . maxValue
        . count
        . Maybe.catMaybes
        . Map.elems
        . filterInfiniteAreas
        . populateAreas
        $ initialSpace
  where
    spaceWidth, spaceHeight :: Int
    spaceWidth  = maximumOf xCoordinate coords
    spaceHeight = maximumOf yCoordinate coords

    zippedCoords :: [(Int, Coordinate)]
    zippedCoords = zip [0 ..] coords

    borderCoords :: [Coordinate]
    borderCoords = mconcat
        [ Coordinate . (, 0) <$> [0 .. spaceWidth]
        , Coordinate . (, spaceHeight - 1) <$> [0 .. spaceWidth]
        , Coordinate . (0, ) <$> [0 .. spaceHeight]
        , Coordinate . (spaceWidth - 1, ) <$> [0 .. spaceHeight]
        ]

    initialSpace :: Space (Maybe Int)
    initialSpace = foldl'
        (\accum (i, coord) -> insertSpace coord (Just i) accum)
        (fillSpace Nothing $ Rect 0 0 spaceWidth spaceHeight)
        zippedCoords

    populateAreas :: Space (Maybe Int) -> Space (Maybe Int)
    populateAreas = Map.mapWithKey mapper
      where
        mapper :: Coordinate -> Maybe Int -> Maybe Int
        mapper _ (Just i) = Just i
        mapper coord Nothing =
            case singleMinimumOn (manhattanDistance coord . snd) zippedCoords of
                Nothing     -> Nothing
                Just (i, _) -> Just i

    filterInfiniteAreas :: Space (Maybe Int) -> Space (Maybe Int)
    filterInfiniteAreas space = Map.filter
        (maybe True $ flip Set.notMember infiniteAreas)
        space
      where
        infiniteAreas :: Set.Set Int
        infiniteAreas =
            Set.fromList
                . Maybe.mapMaybe (join . flip Map.lookup space)
                $ borderCoords


{-# ANN partTwo "HLint: ignore Reduce duplication" #-}
partTwo :: [Coordinate] -> Int
partTwo coords =
    Map.size . Map.filter (< Sum 10000) . populateDistances $ initialSpace
  where
    spaceWidth, spaceHeight :: Int
    spaceWidth  = maximumOf xCoordinate coords
    spaceHeight = maximumOf yCoordinate coords

    initialSpace :: Space (Sum Int)
    initialSpace = fillSpace (Sum 0) $ Rect 0 0 spaceWidth spaceHeight

    populateDistances :: Space (Sum Int) -> Space (Sum Int)
    populateDistances = Map.mapWithKey mapper
      where
        mapper :: Coordinate -> Sum Int -> Sum Int
        mapper coord s = s <> foldMap (Sum . manhattanDistance coord) coords


type Space a = Map Coordinate a


fillSpace :: a -> Rect -> Space a
fillSpace a rect = Map.fromList
    [ (Coordinate (x, y), a)
    | x <- [xmin rect .. xmin rect + width rect]
    , y <- [ymin rect .. ymin rect + height rect]
    ]


insertSpace :: Coordinate -> a -> Space a -> Space a
insertSpace = Map.insert


data Rect = Rect
    { xmin   :: Int
    , ymin   :: Int
    , width  :: Int
    , height :: Int
    } deriving Show


newtype Coordinate = Coordinate (Int, Int) deriving (Show, Eq, Ord)


xCoordinate, yCoordinate :: Coordinate -> Int
xCoordinate (Coordinate (x, _)) = x
yCoordinate (Coordinate (_, y)) = y


manhattanDistance :: Coordinate -> Coordinate -> Int
manhattanDistance (Coordinate (x1, y1)) (Coordinate (x2, y2)) =
    abs (x1 - x2) + abs (y1 - y2)


parseCoordinates :: Text -> Either String [Coordinate]
parseCoordinates =
    Parse.parseOnly $ coordinateParser `Parse.sepBy` Parse.endOfLine
  where
    coordinateParser :: Parse.Parser Coordinate
    coordinateParser = Coordinate <$> xyParser

    xyParser :: Parse.Parser (Int, Int)
    xyParser =
        (,)
            <$> Parse.decimal
            <*> (Parse.char ',' *> Parse.skipSpace *> Parse.decimal)


maxValue :: Ord v => Map k v -> Maybe v
maxValue m = case Map.elems m of
    [] -> Nothing
    vs -> Just (List.maximum vs)


count :: (Foldable f, Ord a) => f a -> Map a (Sum Int)
count = foldl' (\accum a -> Map.insertWith (<>) a (Sum 1) accum) Map.empty


singleMinimumOn :: Ord b => (a -> b) -> [a] -> Maybe a
singleMinimumOn f as = case List.sortOn snd (fmap (\a -> (a, f a)) as) of
    ((a, b) : (_, b') : _) | b /= b'   -> Just a
                           | otherwise -> Nothing
    _ -> Nothing


maximumOf :: Ord b => (a -> b) -> [a] -> b
maximumOf f = f . List.maximumBy (comparing f)
