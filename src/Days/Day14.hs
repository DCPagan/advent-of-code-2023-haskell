{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Days.Day14 (runDay) where

import Control.Arrow
import Control.Lens hiding (cons,uncons,snoc,unsnoc)
import Control.Monad
import Control.Monad.Combinators
import Control.Monad.Primitive

import Data.Array.IArray
import Data.Array.MArray
import Data.Array.ST
import Data.Attoparsec.Text (Parser,anyChar,char,endOfLine)
import Data.Coerce
import Data.Function
import Data.Functor
import Data.Functor.Contravariant
import Data.List
import Data.List.Extra (unsnoc)
import Data.List.Split (splitWhen)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Tuple.Extra (uncurry3)

import qualified Program.RunDay as R (Day,runDay)

import qualified Text.ParserCombinators.ReadP as R
import qualified Text.ParserCombinators.ReadPrec as RP
import Text.Read

import qualified Util.Util as U
import Data.Semigroup (stimesMonoid)

------------ TYPES ------------
data Rock where
  Error :: Rock -- ¿
  Round :: Rock -- O
  Cube :: Rock  -- #
  Space :: Rock -- .
  deriving (Enum,Eq,Ord)

makePrisms ''Rock

instance Read Rock where
  readPrec = do
    rock <- toRock <$> RP.get
    guard (rock /= Error) $> rock

  readListPrec = many readPrec

instance Show Rock where
  show = (: []) . fromRock

  showList ls s = fmap fromRock ls ++ s

toRock :: Char -> Rock
toRock 'O' = Round
toRock '#' = Cube
toRock '.' = Space
toRock _ = Error

fromRock :: Rock -> Char
fromRock Round = 'O'
fromRock Cube = '#'
fromRock Space = '.'
fromRock Error = '¿'

type Coord = (Word, Word)

newtype Grid = Grid { unGrid :: Array Coord Rock }
  deriving (Eq, Ord)

instance Read Grid where
  readPrec = toGrid <$> some row
    where
      cell = toRock <$> RP.get

      row = some cell <* endOfLine

      endOfLine = lift $ R.char '\n' $> () <|> R.string "\r\n" $> ()

  readListPrec = sepBy readPrec endOfLine
    where
      endOfLine = lift $ R.char '\n' $> () <|> R.string "\r\n" $> ()

instance Show Grid where
  show = intercalate "\n" . fmap show . fromGrid

  showList ls s = intercalate "\n\n" (fmap show ls) ++ s

toGrid :: [[Rock]] -> Grid
toGrid = Grid
  <<< (array =<< bounds')
  <<< join . zipWith (fmap . first . (,)) (enumFrom 0) . fmap (zip (enumFrom 0))
  where
    bounds' a = fromMaybe ((0, 1), (0, 0)) $ do
      head <- fst . fst <$> uncons a
      last <- fst . snd <$> unsnoc a
      return (head, last)

fromGrid :: Grid -> [[Rock]]
fromGrid = fmap (fmap snd) . groupBy (on (==) $ fst . fst) . assocs . unGrid

data Direction where
  North :: Direction
  West :: Direction
  South :: Direction
  East :: Direction
  deriving (Bounded, Eq, Enum, Ord, Show)

makePrisms ''Direction

type Cache = M.Map Grid Word

type Input = Grid

type OutputA = Word

type OutputB = Maybe Word

------------ PARSER ------------
rock :: Parser Rock
rock = do
  r <- toRock <$> anyChar
  guard (r /= Error) $> r

row :: Parser [Rock]
row = many rock <* endOfLine

grid :: Parser Grid
grid = toGrid <$> many row

inputParser :: Parser Input
inputParser = grid

------------ PART A ------------
getCubesByLongitude :: Grid -> [[Coord]]
getCubesByLongitude = groupBy (on (==) snd) . sortBy comparison . map fst
  . filter ((Cube ==) . snd) . assocs . unGrid
  where
    comparison = getComparison $ (snd >$< defaultComparison)
      <> (fst >$< defaultComparison)

load :: Grid -> Word -> Word
load = (-) . (1 +) . uncurry subtract <<< each %~ fst <<< bounds . unGrid

columnsSplitByCube :: Grid -> [[[(Coord, Rock)]]]
columnsSplitByCube = fmap (splitWhen ((Cube ==) . snd))
  . groupBy sameLongitude . sortBy comparison . assocs . unGrid
  where
    sameLongitude = on (==) $ snd . fst
    comparison = getComparison
      $ (snd . fst >$< defaultComparison)
      <> (fst . fst >$< defaultComparison)

loadOfTiltedSegment :: Grid -> [(Coord, Rock)] -> Word
loadOfTiltedSegment g = coerce . foldMap (Sum . load g . fst . fst)
  <<< take =<< lengthOf (traverse . _2 . filteredBy _Round)

loadOfTiltedGrid :: Grid -> Word
loadOfTiltedGrid g = coerce . foldMap (foldMap (Sum . loadOfTiltedSegment g))
  . columnsSplitByCube $ g

partA :: Input -> OutputA
partA = loadOfTiltedGrid

------------ PART B ------------
loadOfGrid :: Grid -> Word
loadOfGrid (Grid g) = coerce
  . foldMapOf (to assocs . traverse . filteredBy (_2 . _Round) . _1 . _1)
  (Sum . ((-) . succ . fst . snd . bounds $ g)) $ g

splitColumnsInDirection :: (PrimMonad m, MArray a Rock m)
  => Direction
  -> a Coord Rock
  -> m [[[(Coord, Rock)]]]
splitColumnsInDirection d =
  fmap (fmap (splitWhen ((Cube ==) . snd)) . groupLines . groupByLatitude)
    . getAssocs
  where
    groupByLatitude = groupBy $ on (==) $ fst . fst
    groupLines = case d of
      North -> transpose
      West -> id
      South -> fmap reverse . transpose
      East -> fmap reverse

tiltSegment :: [(Coord, Rock)] -> [(Coord, Rock)]
tiltSegment = zip <$> fmap fst <*> uncurry (++) . partition (Round ==) . fmap snd

tilt :: (PrimMonad m, MArray a Rock m)
  => Direction
  -> a Coord Rock
  -> m (a Coord Rock)
tilt d g = do
  splitAssocs <- splitColumnsInDirection d g
  let tiltedAssocs = splitAssocs >>= (>>= tiltSegment)
  mapM_ (uncurry $ writeArray g) tiltedAssocs
  return g

tiltGrid :: Direction -> Grid -> Grid
tiltGrid d (Grid g) = Grid $ runSTArray $ thaw g >>= tilt d

spin :: (PrimMonad m, MArray a Rock m)
  => a Coord Rock
  -> m (a Coord Rock)
spin = foldr ((>=>) . tilt) return $ enumFrom minBound

spinGrid :: Grid -> Grid
spinGrid (Grid g) = Grid $ runSTArray $ thaw g >>= spin

spins :: Grid -> [Grid]
spins = iterate spinGrid

spinsCached :: Grid -> [(Grid, Cache, Word)]
spinsCached = iterate go . (, mempty, 0)
  where
    go (g, c, i) = (spinGrid g, M.insertWith (const id) g i c, succ i)

firstSpinDuplicate :: Grid -> Maybe (Word, Word, Cache)
firstSpinDuplicate = preview
  $ to spinsCached . traverse . to (\(g, c, i) -> (,i,c) <$> c M.!? g) . _Just

billionthSpin :: Word -> Word -> Cache -> Maybe Grid
billionthSpin i j = fmap fst
  . ifind (\_ n -> i + on mod (subtract i) 1_000_000_000 j == n)

billionthLoad :: Grid -> Maybe Word
billionthLoad =
  firstSpinDuplicate >=> uncurry3 billionthSpin >>> fmap loadOfGrid

spinCycle :: (PrimMonad m, MArray a Rock m)
  => Word
  -> a Coord Rock
  -> m (a Coord Rock)
spinCycle 0 g = return g
spinCycle n g = spinCycle (n - 1) g >>= spin

spinCycleGrid :: Word -> Grid -> Grid
spinCycleGrid 0 g = g
spinCycleGrid n (Grid g) = Grid $ runSTArray $ thaw g >>= spinCycle n

partB :: Input -> OutputB
partB = billionthLoad

runDay :: R.Day
runDay = R.runDay inputParser partA partB
