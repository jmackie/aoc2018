module Day3 (printAnswer) where

import Prelude hiding (id)

import qualified Data.Attoparsec.Text as Parse
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text.IO as Text (readFile)
import qualified Paths_aoc2018 as Paths

import Data.Function (on, (&))
import Data.Maybe (mapMaybe)
import Data.Text (Text)


getQuestionInput :: IO Text
getQuestionInput = Text.readFile =<< Paths.getDataFileName "data/day3.txt"


printAnswer :: IO ()
printAnswer = do
    input  <- getQuestionInput
    claims <- parseClaims input & either error pure

    putStr "\tPart 1: "
    print (overlappingClaims claims) -- 116491

    putStr "\tPart 2: "
    print (nonOverlappingClaims claims) -- Just [707]


overlappingClaims :: [Claim] -> Int
overlappingClaims =
    patchSize . foldMap (uncurry patchOverlap) . combinationsWith claimPatch


nonOverlappingClaims :: [Claim] -> [Int]
nonOverlappingClaims =
    maybe [] (mapMaybe $ uncurry checkNonOverlapping) . others
  where
    checkNonOverlapping :: Claim -> [Claim] -> Maybe Int
    checkNonOverlapping c [] = Just (claimId c)
    checkNonOverlapping c (c' : rest) | not (hasOverlap c c') = Nothing
                                      | otherwise = checkNonOverlapping c rest

    hasOverlap :: Claim -> Claim -> Bool
    hasOverlap = (null .) . patchOverlap `on` claimPatch


data Claim = Claim { claimId :: Int, claimPatch :: Patch Int }


instance Eq Claim where
    (==) = (==) `on` claimId


type Patch a = Set.Set (a, a)


mkPatch :: (Num a, Enum a, Ord a) => a -> a -> a -> a -> Patch a
mkPatch xmin ymin width height = Set.fromList [ (x, y) | x <- xs, y <- ys ]
  where
    xs = [xmin .. xmin + (width - 1)]
    ys = [ymin .. ymin + (height - 1)]


patchOverlap :: Ord a => Patch a -> Patch a -> Patch a
patchOverlap = Set.intersection


patchSize :: Patch a -> Int
patchSize = Set.size


parseClaims :: Text -> Either String [Claim]
parseClaims = Parse.parseOnly claimsParser
  where
    claimsParser :: Parse.Parser [Claim]
    claimsParser = claimParser `Parse.sepBy` Parse.endOfLine


claimParser :: Parse.Parser Claim
claimParser = do
    id <- Parse.char '#' *> Parse.decimal
    Parse.skipSpace *> Parse.char '@' *> Parse.skipSpace
    (left, top) <- parsePair ',' Parse.decimal
    Parse.char ':' *> Parse.skipSpace
    (width, height) <- parsePair 'x' Parse.decimal
    let patch = mkPatch left top width height
    pure (Claim id patch)
  where
    parsePair :: Char -> Parse.Parser a -> Parse.Parser (a, a)
    parsePair sep parser = (,) <$> parser <*> (Parse.char sep *> parser)


combinationsWith :: (a -> b) -> [a] -> [(b, b)]
combinationsWith f l = [ (f x, f y) | (x : ys) <- List.tails l, y <- ys ]


others :: [a] -> Maybe [(a, [a])]
others as = traverse (List.uncons . flip rotate as) [0 .. length as]


rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs
