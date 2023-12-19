{-# LANGUAGE TemplateHaskell #-}
module Days.Day05 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative
import Control.Lens
import Data.Ix
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

------------ TYPES ------------
data MapRange where
  MapRange :: {
    _dstStart :: Int,
    _srcStart :: Int,
    _rng :: Int
  } -> MapRange
  deriving (Eq, Show)
makeLenses ''MapRange

data Mapping where
  Mapping :: {
    _dst :: String,
    _src :: String,
    _maps :: [MapRange]
  } -> Mapping
  deriving (Eq, Show)
makeLenses ''Mapping

data Almanac where
  Almanac :: {
    _seeds :: [Int],
    _mappings :: [Mapping]
  } -> Almanac
  deriving (Eq, Show)
makeLenses ''Almanac

type Input = Almanac

type OutputA = Int

type OutputB = Void

------------ PARSER ------------
parseSeeds :: Parser [Int]
parseSeeds = do
  string "seeds:"
  skipSpace
  sort <$> sepBy decimal skipSpace

parseMapRange :: Parser MapRange
parseMapRange = do
  _dstStart <- decimal 
  skipSpace
  _srcStart <- decimal
  skipSpace
  _rng <- decimal
  return MapRange { _dstStart, _srcStart, _rng }

parseMapping :: Parser Mapping
parseMapping = do
  _dst <- some letter
  string "-to-"
  _src <- some letter
  string " map:"
  endOfLine
  _maps <- sortOn _srcStart <$> sepBy parseMapRange endOfLine
  return Mapping { _dst, _src, _maps }

parseAlmanac :: Parser Almanac
parseAlmanac = do
  _seeds <- parseSeeds
  some endOfLine
  _mappings <- sepBy parseMapping $ some endOfLine
  return Almanac { _seeds, _mappings }

inputParser :: Parser Input
inputParser = parseAlmanac

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PART A ------------
seedInRange :: MapRange -> Int -> Bool
seedInRange = curry inRange
  <$> _srcStart 
  <*> ((+) <$> _srcStart <*> pred . _rng)

mapSeedWithRange :: MapRange -> Int -> Maybe Int
mapSeedWithRange mapRange seed
  | seedInRange mapRange seed = Just $ seed
    + view dstStart mapRange
    - view srcStart mapRange
  | otherwise = Nothing

mapSeedWithMapping :: Mapping -> Int -> Int
mapSeedWithMapping m seed = fromMaybe seed . listToMaybe . catMaybes
  . fmap (flip mapSeedWithRange seed)
  . view maps $ m

mapSeed :: Almanac -> Int -> Int
mapSeed = au (_Wrapping Dual . _Wrapping Endo)
  (foldMapOf $ mappings . traverse . to mapSeedWithMapping)

partA :: Input -> OutputA
partA = minimum . (map <$> mapSeed <*> view seeds)

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
