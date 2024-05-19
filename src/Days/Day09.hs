{-# LANGUAGE DeriveFunctor #-}
module Days.Day09 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative
import Control.Lens
import Data.Fix
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text as P
import Data.Void
{- ORMOLU_ENABLE -}

------------ TYPES ------------
data OasisF a b where
  Zero :: OasisF a b
  Reading :: a -> b -> OasisF a b
  deriving (Eq, Show, Functor)

type Oasis a = Fix (OasisF a)

type Input = [[Int]]

type OutputA = Int

type OutputB = Int

------------ PARSER ------------
parseOasisReading :: Parser [Int]
parseOasisReading = reverse
  <$> P.sepBy (P.signed P.decimal) (P.takeWhile P.isHorizontalSpace)
  <* P.endOfLine

inputParser :: Parser Input
inputParser = many parseOasisReading

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PART A ------------
diffs :: [Int] -> OasisF Int [Int]
diffs [] = Zero
diffs x
  | all (0 ==) x = Zero
  | otherwise = Reading <$> head <*> (zipWith (-) <*> tail) $ x

next :: OasisF Int Int -> Int
next Zero = 0
next (Reading a b) = a + b

partA :: Input -> OutputA
partA = auf (_Wrapping Sum) (foldMapOf traverse) (refold next diffs)

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
