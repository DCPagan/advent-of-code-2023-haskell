{-# LANGUAGE TemplateHaskell #-}
module Days.Day04 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Lens
import Data.Attoparsec.Text
import Data.Ix (inRange)
import Data.List
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Void

import qualified Program.RunDay as R (runDay, Day)
{- ORMOLU_ENABLE -}

------------ TYPES ------------
data Card where
  Card :: {
    _serialId :: Int,
    _winning :: [Int],
    _holding :: [Int]
  } -> Card
  deriving (Eq, Show)
makeLenses ''Card

type Input = [Card]

type OutputA = Int

type OutputB = Int

------------ PARSER ------------
divisor :: Parser ()
divisor = do
  skipSpace
  char '|'
  skipSpace

parseCard :: Parser Card
parseCard = do
  string "Card"
  skipSpace
  _serialId <- decimal
  char ':'
  skipSpace
  _winning <- sort <$> sepBy decimal skipSpace
  divisor
  _holding <- sort <$> sepBy decimal skipSpace
  return Card { _serialId, _winning, _holding }

runDay :: R.Day
runDay = R.runDay inputParser partA partB

inputParser :: Parser Input
inputParser = sepBy parseCard endOfLine

------------ PART A ------------
sortedIntersect :: Ord a => [a] -> [a] -> [a]
sortedIntersect xs ys = sortedIntersect' [] xs ys
  where
    sortedIntersect' k [] _ = k
    sortedIntersect' k _ [] = k
    sortedIntersect' k (a:as) (b:bs)
      | a < b = sortedIntersect' k as (b:bs)
      | a == b = sortedIntersect' (a:k) as bs
      | a > b = sortedIntersect' k (a:as) bs

winIntersect :: Card -> [Int]
winIntersect = sortedIntersect <$> view winning <*> view holding

doubleIntersections :: Int -> Int
doubleIntersections x = if x > 0 then 2 ^ (x - 1) else 0

winningPoints :: Card -> Int
winningPoints = doubleIntersections . length . winIntersect

partA :: Input -> OutputA
partA = alaf Sum foldMap winningPoints

------------ PART B ------------
toWinAlloc :: Card -> (Int, Int)
toWinAlloc = (,) <$> view serialId <*> length . winIntersect

toWinMap :: [Card] -> Map.Map Int Int
toWinMap = Map.fromAscList . map toWinAlloc

toStartCardCount :: [Card] -> Map.Map Int Int
toStartCardCount = Map.fromList . map ((,) <$> view serialId <*> const 1)

addCards :: Int -> Map.Map Int Int -> Int -> Map.Map Int Int
addCards i counts w = itraversed
  {- . ifiltered (\k v -> k > i && k <= i + w) -}
  . ifiltered ((inRange (succ i, i+w) .) . const)
  %~ (Map.findWithDefault 0 i counts +) $ counts

toCountMap :: [Card] -> Map.Map Int Int
toCountMap = ifoldl addCards <$> toStartCardCount <*> toWinMap
  
partB :: Input -> OutputB
partB = sum . Map.elems . toCountMap
