{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
module Day7
    ( printAnswer
    )
where

import Prelude

import qualified Data.Attoparsec.Text as Parse
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.IO as Text (readFile)
import qualified Paths_aoc2018 as Paths

import Control.Applicative (liftA2)
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Map (Map)
import Data.Maybe (fromMaybe, isNothing)
import Data.Set (Set, (\\))
import Data.Text (Text)


getQuestionInput :: IO Text
getQuestionInput = Text.readFile =<< Paths.getDataFileName "data/day7.txt"


printAnswer :: IO ()
printAnswer = do
    input <- getQuestionInput
    steps <- parseSteps input & either error pure

    putStr "\tPart 1: "
    print (partOne steps)
    -- "BHRTWCYSELPUVZAOIJKGMFQDXN"

    putStr "\tPart 2: "
    print (partTwo steps)
    -- 959


partOne :: [Step] -> String
partOne = reverse . go "" . mkProcess
  where
    go :: String -> Process -> String
    go result process =
        case advanceProcess process result (singleton . minimum) of
            Just ([next], process') -> go (next : result) process'
            _                       -> result


partTwo :: [Step] -> Int
partTwo steps =
    let (jobs, initialProcess) =
            getAvailableJobs nworkers mempty (mkProcess steps)
        (initialPool, initialJobs) = allocateJobs (emptyPool nworkers) jobs
    in  snd (go 0 mempty initialJobs initialPool initialProcess)
  where
    nworkers :: Int
    nworkers = 5

    go :: Int -> String -> [Job] -> Pool -> Process -> (String, Int)
    go timer result jobs pool process
        | all isNothing pool = (result, timer)
        | otherwise = case runPool pool of
            ([], pool') -> go (succ timer) result jobs pool' process
            (finished, pool') ->
                let result' = result <> finished

                    (newJobs, process') =
                        getAvailableJobs (countIdle pool') result' process

                    (pool'', jobs') = allocateJobs pool' (jobs <> newJobs)
                in  go (succ timer) result' jobs' pool'' process'

    getAvailableJobs :: Int -> String -> Process -> ([Job], Process)
    getAvailableJobs n result process =
        case advanceProcess process result (take n) of
            Nothing             -> ([], process)
            Just (cs, process') -> (fmap mkJob cs, process')


type Pool = Map Worker (Maybe Job)


emptyPool :: Int -> Pool
emptyPool n = Map.fromList $ zip (fmap Worker [1 .. n]) (repeat Nothing)


countIdle :: Pool -> Int
countIdle = Map.size . Map.filter isNothing


runPool :: Pool -> (String, Pool)
runPool = ([], ) >>= Map.foldlWithKey f
  where
    f :: (String, Pool) -> Worker -> Maybe Job -> (String, Pool)
    f accum          _      Nothing    = accum
    f (result, pool) worker (Just job) = case runJob job of
        Left  job' -> (result, Map.insert worker (Just job') pool)
        Right c    -> (c : result, Map.insert worker Nothing pool)


allocateJobs :: Pool -> [Job] -> (Pool, [Job])
allocateJobs pool = go (Map.toList pool)
  where
    go :: [(Worker, Maybe Job)] -> [Job] -> (Pool, [Job])
    go []        remaining = (Map.empty, remaining)
    go remaining []        = (Map.fromList remaining, [])

    go ((worker, Nothing) : workers) (job : jobs) =
        first (Map.insert worker (Just job)) (go workers jobs)

    go (busy : workers) jobs =
        first (uncurry Map.insert busy) (go workers jobs)


newtype Worker = Worker Int deriving newtype (Eq, Ord)


data Job = Job Char Int


mkJob :: Char -> Job
mkJob c = Job c (Char.ord c - 4)


runJob :: Job -> Either Job Char
runJob (Job c t) = if t <= 1 then Right c else Left (Job c (pred t))


data Process = Process
    { _processSource   :: StepMap
    , processRemaining :: StepMap
    , processPending   :: Set Char
    }


mkProcess :: [Step] -> Process
mkProcess steps =
    let stepMap = mkStepMap steps
    in  Process stepMap
                stepMap
                (Map.keysSet stepMap \\ mconcat (Map.elems stepMap))


advanceProcess
    :: Process -> String -> (String -> String) -> Maybe (String, Process)
advanceProcess process@(Process stepMap remaining pending) accum select
    | Set.null pending
    = Nothing
    | otherwise
    = let
          selection    = select (filter isReady $ Set.toList pending)
          selectionSet = Set.fromList selection
          nextProcess  = process
              { processRemaining = remaining `Map.withoutKeys` selectionSet
              , processPending   =
                  (pending \\ selectionSet)
                      <> foldMap
                             (\k -> lookupWithDefault Set.empty k remaining)
                             selectionSet
              }
      in
          Just (selection, nextProcess)
  where
    isReady :: Char -> Bool
    isReady c = all (`elem` accum) (dependenciesFor c stepMap)


type StepMap = Map Char (Set Char)


dependenciesFor :: Char -> StepMap -> String
dependenciesFor c = Map.keys . Map.filter (Set.member c)


mkStepMap :: [Step] -> StepMap
mkStepMap = foldl' addStep Map.empty
  where
    addStep :: StepMap -> Step -> StepMap
    addStep accum (before :-> after) = Map.unionWith
        (<>)
        (Map.fromList [(before, Set.singleton after), (after, Set.empty)])
        accum


data Step = Char :-> Char  -- lhs must happen before rhs


parseSteps :: Text -> Either String [Step]
parseSteps = Parse.parseOnly steps
  where
    steps :: Parse.Parser [Step]
    steps = step `Parse.sepBy` Parse.endOfLine

    step :: Parse.Parser Step
    step = do
        a <- Parse.string "Step " *> capitalLetter
        b <- Parse.string " must be finished before step " *> capitalLetter
        _ <- Parse.string " can begin."
        pure (a :-> b)

    capitalLetter :: Parse.Parser Char
    capitalLetter = Parse.satisfy (liftA2 (&&) Char.isAlpha Char.isUpper)


lookupWithDefault :: Ord k => v -> k -> Map k v -> v
lookupWithDefault def k m = fromMaybe def (Map.lookup k m)


singleton :: a -> [a]
singleton = (: [])


_example :: [Step]
_example =
    [ 'C' :-> 'A'
    , 'C' :-> 'F'
    , 'A' :-> 'B'
    , 'A' :-> 'D'
    , 'B' :-> 'E'
    , 'D' :-> 'E'
    , 'F' :-> 'E'
    ]
