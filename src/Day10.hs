{-# LANGUAGE OverloadedStrings #-}
module Day10
    ( printAnswer
    )
where

import Prelude

import qualified Data.Attoparsec.Text as Parse
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text.IO as Text (readFile)
import qualified Paths_aoc2018 as Paths

import Data.Bifunctor (second)
import Data.Function ((&))
import Data.Set (Set)
import Data.Text (Text)


getQuestionInput :: IO Text
getQuestionInput = Text.readFile =<< Paths.getDataFileName "data/day10.txt"


printAnswer :: IO ()
printAnswer = do
    input  <- getQuestionInput
    points <- parsePoints input & either error pure

    case solution points of
        Just (partOne, partTwo) -> do
            putStrLn "\tPart 1:"
            putStrLn partOne

            putStr "\tPart 2: "
            print partTwo

        Nothing -> undefined


solution :: [Point] -> Maybe (String, Int)
solution points = do
    let (positiveFrames, n) = dropWhile' (not . allPositive) frames
    (nadir, n') <- findNadir positiveFrames
    pure (drawPoints nadir, n + n')
  where
    frames :: [[Point]]
    frames = (\dt -> movePoint dt <$> points) <$> [0 ..]

    allPositive :: [Point] -> Bool
    allPositive = all $ \(Point (x, y) _) -> x >= 0 && y >= 0

    findNadir :: [[Point]] -> Maybe ([Point], Int)
    findNadir []  = Nothing
    findNadir [_] = Nothing
    findNadir (ps : ps' : rest)
        | boxSize ps < boxSize ps' = Just (ps, 0)
        | otherwise                = second (+ 1) <$> findNadir (ps' : rest)

    boxSize :: [Point] -> Int
    boxSize ps = (xmax - xmin) * (ymax - ymin)
      where
        (xs, ys) = unzip (fmap pointPosition ps)
        xmin     = List.minimum xs
        xmax     = List.maximum xs
        ymin     = List.minimum ys
        ymax     = List.maximum ys


data Point = Point
    {  pointPosition :: (Int, Int)
    , _pointVelocity :: (Int, Int)
    }


movePoint :: Int -> Point -> Point
movePoint dt (Point (x, y) (vx, vy)) =
    Point (x + vx * dt, y + vy * dt) (vx, vy)


drawPoints :: [Point] -> String
drawPoints = draw . Set.fromList . fmap pointPosition . rescalePoints
  where
    draw :: Set (Int, Int) -> String
    draw positions = unlines
        [ [ if Set.member (x, y) positions then '#' else '.'
          | x <- [0 .. xmax]
          ]
        | y <- [0 .. ymax]
        ]
      where
        xmax = Set.findMax (Set.map fst positions)
        ymax = Set.findMax (Set.map snd positions)


rescalePoints :: [Point] -> [Point]
rescalePoints points = fmap
    (\(Point (x, y) velocity) -> Point (x - xmin, y - ymin) velocity)
    points
  where
    xmin = List.minimum (fst . pointPosition <$> points)
    ymin = List.minimum (snd . pointPosition <$> points)


parsePoints :: Text -> Either String [Point]
parsePoints = Parse.parseOnly points
  where
    points :: Parse.Parser [Point]
    points = Parse.many1 point

    point :: Parse.Parser Point
    point = Point <$> position <*> velocity

    position :: Parse.Parser (Int, Int)
    position = do
        _  <- Parse.string "position=" *> openAngle
        px <- Parse.signed Parse.decimal <* comma
        py <- Parse.signed Parse.decimal <* closeAngle
        pure (px, py)

    velocity :: Parse.Parser (Int, Int)
    velocity = do
        _  <- Parse.string "velocity=" *> openAngle
        vx <- Parse.signed Parse.decimal <* comma
        vy <- Parse.signed Parse.decimal <* closeAngle
        pure (vx, vy)

    comma :: Parse.Parser ()
    comma = Parse.char ',' *> Parse.skipSpace

    openAngle :: Parse.Parser ()
    openAngle = Parse.char '<' *> Parse.skipSpace

    closeAngle :: Parse.Parser ()
    closeAngle = Parse.char '>' *> Parse.skipSpace


-- Count how many things were dropped
dropWhile' :: (a -> Bool) -> [a] -> ([a], Int)
dropWhile' _ [] = ([], 0)
dropWhile' f (a : as) | f a       = second (+ 1) (dropWhile' f as)
                      | otherwise = (a : as, 0)
