{-# LANGUAGE TemplateHaskell #-}
module Days.Day06 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative
import Control.Arrow ((***))
import Control.Lens
import Data.Ix (rangeSize)
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

------------ TYPES ------------
data Race where
  Race :: {
    _time :: Int,
    _dist :: Int
  } -> Race
  deriving (Eq, Show)
makeLenses ''Race

type Quadratic a = (a, a)

type QuadraticRoots a = (a, a)

type Input = [Race]

type OutputA = Int

type OutputB = Void

------------ PARSER ------------
parseLine :: Text -> Parser [Int]
parseLine s = do
  string s
  char ':'
  skipSpace
  xs <- sepBy decimal skipSpace
  endOfLine
  return xs

parseTime :: Parser [Int]
parseTime = parseLine "Time"

parseDistance :: Parser [Int]
parseDistance = parseLine "Distance"

parseRaces :: Parser [Race]
parseRaces = zipWith Race <$> parseTime <*> parseDistance

inputParser :: Parser Input
inputParser = parseRaces

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PART A ------------
-- The distance traveled in a race is a quadratic equation with respect to
-- the time spent holding the button; the first coefficient is -1.
toQuadratic :: Floating a => Race -> Quadratic a
toQuadratic (Race t d) = (t, negate d) & each %~ fromIntegral

-- Given that the distances are possible for each race, the roots of these
-- quadratic equations are guaranteed to be real.
solveQuadratic :: Floating a => Quadratic a -> QuadraticRoots a
solveQuadratic (b, c) = ((b - d) / 2, (b + d) / 2)
  where
    d = sqrt $ b * b + 4 * c

locusSize :: Race -> Int
locusSize = rangeSize . (ceiling *** floor) . solveQuadratic . toQuadratic

partA :: Input -> OutputA
partA = product . map locusSize

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
