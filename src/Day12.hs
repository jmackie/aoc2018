{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html
--
module Day12
    ( printAnswer
    )
where

import Prelude

import qualified Data.Attoparsec.Text as Parse
import qualified Data.Text.IO as Text (readFile)
import qualified Data.Universe as Universe
import qualified Paths_aoc2018 as Paths

import Control.Applicative ((<|>))
import Control.Comonad (extend, extract)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text (Text)
import Data.Universe (Universe(Universe))


getQuestionInput :: IO Text
getQuestionInput = Text.readFile =<< Paths.getDataFileName "data/day12.txt"


printAnswer :: IO ()
printAnswer = do
    input                 <- getQuestionInput
    (initialState, notes) <- parseInput input & either error pure

    putStr "\tPart 1: "
    print (solution 20 notes initialState)
    -- 2736

    putStr "\tPart 2: "
    --for_ [100, 110 .. 1000] $ \n -> do
    --    putStr (show n <> ",")
    --    print (solution n notes initialState)
    --
    -- After 300 generations the result is 19805
    -- Each iteration increases the result by 63
    -- Therefore 50_000_000_000 results in 3150000000905
    print (3150000000905 :: Int)


solution :: Int -> [Note] -> NonEmpty Pot -> Int
solution generations notes pots =
    countPlants
        . zoom (length pots) generations
        . foldN generations (evolve notes)
        $ mkPotverse pots


evolve :: [Note] -> Potverse -> Potverse
evolve notes = extend (\u -> extract u $> nextGeneration (fmap snd u))
  where
    nextGeneration :: Universe Pot -> Pot
    nextGeneration u = case filter (isMatch u) notes of
        []            -> EmptyPot
        [Note _ next] -> next
        _             -> undefined  -- should only be one matching Note

    isMatch :: Universe Pot -> Note -> Bool
    isMatch u (Note pattern _) = Universe.view 2 u == pattern


zoom :: Int -> Int -> Potverse -> [(Int, Pot)]
zoom initialWidth generations u =
    let (middle, after) =
            splitAt initialWidth (extract <$> iterate Universe.right u)
        before = drop 1 (extract <$> iterate Universe.left u)
        -- NOTE: for the current rules, each generation can increase the
        -- initial width by 4
        growth = 4 * (generations - 1)
    in  reverse (take growth before) <> middle <> take growth after


countPlants :: [(Int, Pot)] -> Int
countPlants []                     = 0
countPlants ((_, EmptyPot) : rest) = countPlants rest
countPlants ((i, PlantPot) : rest) = i + countPlants rest


type Potverse = Universe (Int, Pot)  -- lol


mkPotverse :: NonEmpty Pot -> Potverse
mkPotverse (p :| ps) = Universe ([-1, -2 ..] `zip` repeat EmptyPot)
                                (0, p)
                                ([1 ..] `zip` (ps <> repeat EmptyPot))


data Pot = EmptyPot | PlantPot deriving Eq


instance Show Pot where
    show EmptyPot = "."
    show PlantPot = "#"


type Pattern = [Pot]


data Note = Note Pattern {- => -} Pot


instance Show Note where
    show (Note pattern nextgen) = show pattern <> " => " <> show nextgen


parseInput :: Text -> Either String (NonEmpty Pot, [Note])
parseInput = Parse.parseOnly $ do
    _            <- Parse.string "initial state: "
    initialState <- pot `Parse.manyTill` Parse.endOfLine >>= \case
        []     -> fail "not enough pots in initial state"
        p : ps -> pure (p :| ps)
    Parse.skipSpace
    notes <- note `Parse.sepBy` Parse.endOfLine
    pure (initialState, notes)
  where
    pot :: Parse.Parser Pot
    pot = (Parse.char '#' $> PlantPot) <|> (Parse.char '.' $> EmptyPot)

    note :: Parse.Parser Note
    note = do
        pattern <- pot `Parse.manyTill` Parse.space
        Parse.string "=>" *> Parse.skipSpace
        nextgen <- pot
        pure (Note pattern nextgen)


foldN :: Int -> (b -> b) -> b -> b
foldN n f b = foldl' (\b' _ -> f b') b [1 .. n]
