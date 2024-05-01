{-# LANGUAGE TemplateHaskell #-}
module Days.Day05 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Either
import Data.Function
import Data.Ix
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text hiding (take, takeWhile)
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

_srcEnd :: MapRange -> Int
_srcEnd s = _srcStart s + _rng s - 1

_dstEnd :: MapRange -> Int
_dstEnd s = _dstStart s + _rng s - 1

_srcEnd' :: MapRange -> Int -> MapRange
_srcEnd' s a = s { _rng = a - _srcStart s + 1 }

_dstEnd' :: MapRange -> Int -> MapRange
_dstEnd' s a = s { _rng = a - _dstStart s + 1 }

srcEnd :: Lens' MapRange Int
srcEnd = lens _srcEnd _srcEnd'

dstEnd :: Lens' MapRange Int
dstEnd = lens _dstEnd _dstEnd'

type Seeds = [Int]

data SeedMap where
  SeedMap :: {
    _dst :: String,
    _src :: String,
    _ranges :: [MapRange]
  } -> SeedMap
  deriving (Eq, Show)
makeLenses ''SeedMap

data Almanac where
  Almanac :: {
    _seeds :: Seeds,
    _maps :: [SeedMap]
  } -> Almanac
  deriving (Eq, Show)
makeLenses ''Almanac

instance Semigroup SeedMap where
  a <> b = SeedMap {
    _src = if null (_src a) then _src b else _src a,
    _dst = if null (_dst b) then _dst a else _dst b,
    _ranges = uncurry maskRanges $ on composeRanges _ranges a b
  }

instance Monoid SeedMap where
  mempty = SeedMap {
    _src = mempty,
    _dst = mempty,
    _ranges = mempty
  }

type RangeComposition = (
  Maybe (Either MapRange MapRange),
  Maybe MapRange,
  Maybe (Either MapRange MapRange))

type Input = Almanac

type OutputA = Int

type OutputB = Int

------------ PARSER ------------
intermediateRange :: MapRange -> MapRange -> MapRange
intermediateRange a b = seedRange
  (succ . _srcEnd $ a) (_srcStart b - _srcEnd a - 1)

totalizeRanges :: [MapRange] -> [MapRange]
totalizeRanges = sortOn _srcStart
  . ((<>) <*> filter ((0 <) . _rng)
    . (zipWith intermediateRange <*> tail)
    . sortOn _srcStart)

parseSeeds :: Parser Seeds
parseSeeds = do
  string "seeds:"
  skipSpace
  sepBy decimal skipSpace

parseMapRange :: Parser MapRange
parseMapRange = do
  _dstStart <- decimal 
  skipSpace
  _srcStart <- decimal
  skipSpace
  _rng <- decimal
  return MapRange { _dstStart, _srcStart, _rng }

parseSeedMap :: Parser SeedMap
parseSeedMap = do
  _dst <- some letter
  string "-to-"
  _src <- some letter
  string " map:"
  endOfLine
  _ranges <- sortOn _srcStart <$> sepBy parseMapRange endOfLine
  return SeedMap { _dst, _src, _ranges }

parseAlmanac :: Parser Almanac
parseAlmanac = do
  _seeds <- parseSeeds
  some endOfLine
  _maps <- sepBy parseSeedMap $ some endOfLine
  return Almanac { _seeds, _maps }

inputParser :: Parser Input
inputParser = parseAlmanac

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PART A ------------
seedInRange :: MapRange -> Int -> Bool
seedInRange = curry inRange <$> _srcStart <*> _srcEnd

mapSeedWithRange :: MapRange -> Int -> Maybe Int
mapSeedWithRange mapRange seed
  | seedInRange mapRange seed = Just $ seed
    + view dstStart mapRange
    - view srcStart mapRange
  | otherwise = Nothing

mapSeedWithSeedMap :: SeedMap -> Int -> Int
mapSeedWithSeedMap m seed = fromMaybe seed . listToMaybe . catMaybes
  . fmap (flip mapSeedWithRange seed) . sortOn _srcStart
  . _ranges $ m

mapSeed :: Almanac -> Int -> Int
mapSeed = au (_Wrapping Dual . _Wrapping Endo)
  (foldMapOf $ maps . traverse . to mapSeedWithSeedMap)

partA :: Input -> OutputA
partA = minimum . (map <$> mapSeed <*> view seeds)

------------ PART B ------------
toPairs :: Seeds -> [[Int]]
toPairs = (map.map) snd
  . groupBy (on (==) (flip div 2 . fst))
  . zip (enumFrom 0)

seedRange :: Int -> Int -> MapRange
seedRange a b = MapRange {
  _srcStart = a,
  _dstStart = a,
  _rng = b
}

pairToRange :: [Int] -> MapRange
pairToRange [a, b] = seedRange a b

seedsToMap :: Seeds -> SeedMap
seedsToMap seeds = SeedMap {
  _src = "seed",
  _dst = "seed",
  _ranges = map pairToRange . toPairs $ seeds
}

addSeedMap :: Almanac -> Almanac
addSeedMap almanac = almanac & maps %~
  (almanac ^. seeds . to seedsToMap <|)

rangeBefore :: MapRange -> MapRange -> Bool
rangeBefore a b = _dstEnd a < _srcStart b

rangeAfter :: MapRange -> MapRange -> Bool
rangeAfter a b = _dstStart a > _srcEnd b

rangesIntersect :: MapRange -> MapRange -> Bool
rangesIntersect a b = not (rangeBefore a b) && not (rangeAfter a b)

srcBefore :: MapRange -> MapRange -> Bool
srcBefore a b = _srcEnd a < _srcStart b

srcAfter :: MapRange -> MapRange -> Bool
srcAfter a b = _srcStart a > _srcEnd b

srcIntersect :: MapRange -> MapRange -> Bool
srcIntersect a b = not (srcBefore a b) && not (srcAfter a b)

{-
 - Lefts are uncomposed ranges from latter range, and may be masked from
 - former ranges.  Rights are composed from former and latter ranges.
 -}
composeRange :: MapRange -> MapRange -> RangeComposition
composeRange a b
  | rangeBefore a b = (Just $ Right a, Nothing, Just $ Left b)
  | rangeAfter a b = (Just $ Left b, Nothing, Just $ Right a)
  | otherwise = (left, middle, right)
    where
      left = case compare lDelta 0 of
        LT -> Just $ Right $ set dstEnd (view srcStart b - 1) a
        GT -> Just $ Left $ set srcEnd (view dstStart a - 1) b
        EQ -> Nothing
      middle = Just MapRange {
        _srcStart = _srcStart a - min lDelta 0,
        _dstStart = _dstStart b + max lDelta 0,
        _rng = _rng a + min lDelta 0 - max rDelta 0
      }
      right = case compare rDelta 0 of
        LT -> Just $ Left right'
        GT -> Just $ Right right'
        EQ -> Nothing
      lDelta = _dstStart a - _srcStart b
      rDelta = _dstEnd a - _srcEnd b
      right' = MapRange {
        _dstStart = _dstEnd (if rDelta > 0 then a else b) - abs rDelta + 1,
        _srcStart = _srcEnd (if rDelta > 0 then a else b) - abs rDelta + 1,
        _rng = abs rDelta
      }

{-
 - Left overflow from the former range is prepended to the composition, from
 - the latter range is prepended to the ranges to be masked.
 -}
handleLeft :: Maybe (Either MapRange MapRange) ->
  ([MapRange], [MapRange]) -> ([MapRange], [MapRange])
handleLeft left composition = maybe composition
  (either
    (\l -> _1 %~ (l:) $ composition)
    (\l -> _2 %~ (l:) $ composition)) left

{-
 - Right overflow from the former range is prepended to the left operand of
 - the composition, from the latter range is prepended to the right operand.
 -}
handleRight :: Maybe (Either a a) -> ([a] -> [a] -> b) -> [a] -> [a] -> b
handleRight right f = maybe f
  (either
    (\r as bs -> f as (r:bs))
    (\r as bs -> f (r:as) bs)) right

{-
 - The second field comprises ranges composed between the former and latter
 - maps.  The first field comprises ranges from the latter map not composed
 - with ranges from the former map.
 -}
composeRanges :: [MapRange] -> [MapRange] -> ([MapRange], [MapRange])
composeRanges [] bs = (bs, [])
composeRanges as [] = ([], as)
composeRanges as bs = cr' (sortOn _dstStart as) (sortOn _srcStart bs)
  where
    cr' [] bs = (bs, [])
    cr' as [] = ([], as)
    cr' (a:as) (b:bs) = case composeRange a b of
      (_, Nothing, _) -> if rangeBefore a b
        then (_2 %~ (a :)) $ cr' as (b:bs)
        else (_1 %~ (b :)) $ cr' (a:as) bs
      (left, Just middle, right) -> handleLeft left . (_2 %~ (middle:))
        $ handleRight right cr' as bs

maskRange :: MapRange -> MapRange -> ([MapRange], [MapRange])
maskRange a b
  | srcIntersect a b = (each %~ sortOn _srcStart)
    . partitionEithers . catMaybes $ [left, middle, right]
  | otherwise = ([], [])
    where
      left = case compare lDelta 0 of
        LT -> Just . Right $ set dstEnd (view srcStart b - 1) a
        GT -> Just . Left $ set srcEnd (view dstStart a - 1) b
        EQ -> Nothing
      middle = Just $ Right a {
        _dstStart = _dstStart a - min lDelta 0,
        _srcStart = _srcStart a - min lDelta 0,
        _rng = _rng a + min lDelta 0 - max rDelta 0
      }
      right = case compare rDelta 0 of
        LT -> Just $ Left right'
        GT -> Just $ Right right'
        EQ -> Nothing
      lDelta = _srcStart a - _srcStart b
      rDelta = _srcEnd a - _srcEnd b
      rightDstStart = _dstEnd (if rDelta < 0 then b else a) - abs rDelta + 1
      rightSrcStart = _srcEnd (if rDelta < 0 then b else a) - abs rDelta + 1
      right' = MapRange {
        _dstStart = rightDstStart,
        _srcStart = rightSrcStart,
        _rng = abs rDelta
      }

maskRanges :: [MapRange] -> [MapRange] -> [MapRange]
maskRanges [] bs = bs
maskRanges as [] = as
maskRanges as' bs' = fromMaybe tailcall $ do
  (aInit, aLast) <- unsnoc ls
  return $ aInit <> rs <> maskRanges (aLast:as) bs
    where
      (a:as) = sortOn _srcStart as'
      (b:bs) = sortOn _srcStart bs'
      (ls, rs) = maskRange a b
      tailcall = if srcBefore a b
        then a : maskRanges as (b:bs)
        else b : maskRanges (a:as) bs

mergeSeedMaps :: Almanac -> SeedMap
mergeSeedMaps = foldOf (maps . traverse)

partB :: Input -> OutputB
partB = minimum1Of (_2 . traverse . dstStart)
  . (composeRanges
    <$> view (seeds . to seedsToMap . ranges)
    <*> view ranges . mergeSeedMaps)
