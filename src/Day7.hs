{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
module Day7
    ( printAnswer
    )
where

import Prelude hiding (head, tail)

import qualified Data.Attoparsec.Text as Parse
import qualified Data.Char as Char
import qualified Data.Digraph as Digraph
import qualified Data.Set as Set
import qualified Data.Text.IO as Text (readFile)
import qualified Paths_aoc2018 as Paths

import Control.Applicative (liftA2)
import Control.Monad (guard)
import Data.Bifunctor (bimap, first)
import Data.Digraph (Arrow((:->)), Digraph(Digraph))
import Data.Function (on, (&))
import Data.Functor ((<&>))
import Data.List (sort, (\\))
import Data.Maybe (catMaybes, fromJust, isNothing)
import Data.Text (Text)


getQuestionInput :: IO Text
getQuestionInput = Text.readFile =<< Paths.getDataFileName "data/day7.txt"


printAnswer :: IO ()
printAnswer = do
    input   <- getQuestionInput
    digraph <- parseDigraph input & either error pure

    putStr "\tPart 1: "
    print (partOne digraph)
    -- "BHRTWCYSELPUVZAOIJKGMFQDXN"

    putStr "\tPart 2: "
    print (partTwo digraph 5)


partOne :: Digraph Char -> String
partOne = fromJust . Digraph.tsortWith compare
--        ^^^^^^^^ Shouldn't be cyclic


partTwo :: Digraph Char -> Int -> Int
partTwo digraph nworkers =
    let workers =
            allocateJobs (replicate nworkers Nothing) (availableJobs mempty)
    in  go 0 workers mempty
  where
    go :: Int -> [Worker] -> String -> Int
    go timer workers done
        | all isNothing workers = timer
        | otherwise = case runWorkers workers of
            (workers', []) -> go (succ timer) workers' done
            (workers', done') ->
                let done''    = done <> done'
                    jobs      = availableJobs done'' \\ catMaybes workers'
                    workers'' = allocateJobs workers' jobs
                in  go (succ timer) workers'' done''

    availableJobs :: String -> [Job]
    availableJobs done = sort $ do
        c <- Set.toList (Digraph.vertices digraph) \\ done
        let needs = Set.toList (Digraph.dipred c digraph)
        guard $ all (`elem` done) needs
        pure (mkJob c)

    allocateJobs :: [Worker] -> [Job] -> [Worker]
    allocateJobs []             _        = []
    allocateJobs ws             []       = ws
    allocateJobs (Nothing : ws) (j : js) = Just j : allocateJobs ws js
    allocateJobs (busy    : ws) js       = busy : allocateJobs ws js

    runWorkers :: [Worker] -> ([Worker], String)
    runWorkers []                = ([], "")
    runWorkers (Nothing  : rest) = first (Nothing :) (runWorkers rest)
    runWorkers (Just job : rest) = case runJob job of
        Left  job' -> first (Just job' :) (runWorkers rest)
        Right c    -> bimap (Nothing :) (c :) (runWorkers rest)


type Worker = Maybe Job


data Job = Job Char Int

instance Eq  Job where (==)    = (==) `on`    \(Job c _) -> c
instance Ord Job where compare = compare `on` \(Job c _) -> c


mkJob :: Char -> Job
mkJob c = Job c (Char.ord c - 4)  -- yolo


runJob :: Job -> Either Job Char
runJob (Job c t) = if t <= 1 then Right c else Left (Job c (pred t))


parseDigraph :: Text -> Either String (Digraph Char)
parseDigraph input =
    Parse.parseOnly (step `Parse.sepBy` Parse.endOfLine) input
        <&> \steps -> Digraph
                (Set.fromList $ concatMap (\(a, b) -> [a, b]) steps)
                (Set.fromList $ fmap (uncurry (:->)) steps)
  where
    step :: Parse.Parser (Char, Char)
    step = do
        a <- Parse.string "Step " *> capitalLetter
        b <- Parse.string " must be finished before step " *> capitalLetter
        _ <- Parse.string " can begin."
        pure (a, b)

    capitalLetter :: Parse.Parser Char
    capitalLetter = Parse.satisfy (liftA2 (&&) Char.isAlpha Char.isUpper)


_example :: Digraph Char
_example = Digraph
    ['A', 'B', 'C', 'D', 'E', 'F']
    [ 'C' :-> 'A'
    , 'C' :-> 'F'
    , 'A' :-> 'B'
    , 'A' :-> 'D'
    , 'B' :-> 'E'
    , 'D' :-> 'E'
    , 'F' :-> 'E'
    ]
