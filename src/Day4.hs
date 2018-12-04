{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Day4 (printAnswer) where

import Prelude

import qualified Data.Attoparsec.Text as Parse
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text.IO as Text (readFile)
import qualified Paths_aoc2018 as Paths

import Control.Monad (foldM)
import Data.Foldable (asum, fold, foldl')
import Data.Function (on, (&))
import Data.Map (Map)
import Data.Monoid (Sum(Sum, getSum))
import Data.Ord (comparing)
import Data.Text (Text)


getQuestionInput :: IO Text
getQuestionInput = Text.readFile =<< Paths.getDataFileName "data/day4.txt"


printAnswer :: IO ()
printAnswer = do
    input   <- getQuestionInput
    records <- parseRecords input & either error pure

    putStr "\tPart 1: "
    print (part1 records)
    -- Just 99759

    putStr "\tPart 2: "
    print (part2 records)
    -- Just 97884


part1 :: [Record] -> Maybe Int
part1 []      = Nothing
part1 records = do
    guardId  <- sleepiestGuard
    sleepMap <- Map.lookup guardId sleepMaps
    minute   <- sleepiestMinute sleepMap
    pure (guardId * minute)
  where
    sleepMaps :: Map GuardId SleepMap
    sleepMaps = mkSleepMaps (groupRecords records)

    sleepiestGuard :: Maybe GuardId
    sleepiestGuard = do
        (guardId, _) <- maximumBy (comparing (getSum . fold . snd))
                                  (Map.toList sleepMaps)
        pure guardId

    sleepiestMinute :: SleepMap -> Maybe Minute
    sleepiestMinute sleepMap = do
        (minute, _) <- maximumBy (comparing snd) (Map.toList sleepMap)
        pure minute


part2 :: [Record] -> Maybe Int
part2 []      = Nothing
part2 records = do
    (guardId, minute, _) <-
        maximumBy (comparing thrd)
        . flattenSleepMaps
        . mkSleepMaps
        . groupRecords
        $ records
    pure (guardId * minute)


mkSleepMaps :: Map GuardId [Record] -> Map GuardId SleepMap
mkSleepMaps = fmap (buildSleepMap Map.empty)
  where
    buildSleepMap :: SleepMap -> [Record] -> SleepMap
    buildSleepMap accum []              = accum
    buildSleepMap accum (_      : []  ) = accum
    buildSleepMap accum (r : r' : rest) = do
        case recordAction r of
            BeginsShift -> buildSleepMap accum (r' : rest)
            WakesUp     -> buildSleepMap accum (r' : rest)
            FallsAsleep ->
                Map.unionWith (<>) (buildSleepMap accum (r' : rest))
                    $ (singletonSleepMap `on` recordMinute) r r'


flattenSleepMaps :: Map GuardId SleepMap -> [(GuardId, Minute, Sum Int)]
flattenSleepMaps sleepMaps =
    [ (guardId, minute, sleepSum)
    | (guardId, sleepMap) <- Map.toList sleepMaps
    , (minute , sleepSum) <- Map.toList sleepMap
    ]


-- Synonyms for clarity
type GuardId  = Int
type Minute   = Int
type SleepMap = Map Minute (Sum Int)


singletonSleepMap :: Minute -> Minute -> SleepMap
singletonSleepMap start stop =
    Map.fromList [ (minute, Sum 1) | minute <- [start .. stop - 1] ]


data Record = Record
    { recordTimestamp :: Timestamp
    , recordGuardId   :: GuardId
    , recordAction    :: Action
    }


recordMinute :: Record -> Minute
recordMinute = timestampMinute . recordTimestamp


data Timestamp = Timestamp
    { timestampYear   :: Int
    , timestampMonth  :: Int
    , timestampDay    :: Int
    , timestampHour   :: Int
    , timestampMinute :: Minute
    } deriving (Eq)


instance Ord Timestamp where
    compare = comparing timestampYear <>
              comparing timestampMonth <>
              comparing timestampDay <>
              comparing timestampHour <>
              comparing timestampMinute


data Action
    = BeginsShift
    | FallsAsleep
    | WakesUp


groupRecords :: [Record] -> Map GuardId [Record]
groupRecords = fmap (List.sortOn recordTimestamp) . buildMap
    (\accum record -> Map.insertWith (<>) (recordGuardId record) [record] accum)


parseRecords :: Text -> Either String [Record]
parseRecords input = do
    timestamps <- List.sortOn fst <$> Parse.parseOnly initialLineParser input
    --                 ^^^^^^ important!
    case timestamps of
        []                              -> Left "no input lines"
        ((timestamp, rawAction) : rest) -> do
            (mbGuardId, action) <- Parse.parseOnly actionParser rawAction
            case mbGuardId of
                Nothing      -> Left "first record is missing a guard id"
                Just guardId -> do
                    (_, records) <- foldM parseActions (guardId, []) rest
                    pure (Record timestamp guardId action : reverse records)
  where
    parseActions
        :: (GuardId, [Record])
        -> (Timestamp, Text)
        -> Either String (GuardId, [Record])
    parseActions (guardId, accum) (timestamp, rawAction) = do
        (mbGuardId, action) <- Parse.parseOnly actionParser rawAction
        case mbGuardId of
            Nothing -> pure (guardId, Record timestamp guardId action : accum)
            Just newGuardId ->
                pure (newGuardId, Record timestamp newGuardId action : accum)

    actionParser :: Parse.Parser (Maybe GuardId, Action)
    actionParser = asum
        [ (Nothing, WakesUp) <$ Parse.string "wakes up"
        , (Nothing, FallsAsleep) <$ Parse.string "falls asleep"
        , do
            _       <- Parse.string "Guard #"
            guardId <- Parse.decimal
            _       <- Parse.skipSpace *> Parse.string "begins shift"
            pure (Just guardId, BeginsShift)
        ]

    initialLineParser :: Parse.Parser [(Timestamp, Text)]
    initialLineParser = Parse.many' $ do
        _         <- Parse.char '['
        timestamp <- timestampParser
        _         <- Parse.char ']'
        Parse.skipSpace
        rest <- Parse.takeTill Parse.isEndOfLine <* Parse.endOfLine
        pure (timestamp, rest)

    timestampParser :: Parse.Parser Timestamp
    timestampParser =
        Timestamp
            <$> Parse.decimal
            <*> (Parse.char '-' *> Parse.decimal)
            <*> (Parse.char '-' *> Parse.decimal)
            <*> (Parse.skipSpace *> Parse.decimal)
            <*> (Parse.char ':' *> Parse.decimal)


buildMap :: Foldable t => (Map k v -> a -> Map k v) -> t a -> Map k v
buildMap f = foldl' f Map.empty


maximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
maximumBy _ [] = Nothing
maximumBy f as = Just (List.maximumBy f as)


thrd :: (a, b, c) -> c
thrd (_, _, c) = c
