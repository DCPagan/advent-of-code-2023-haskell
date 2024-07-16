{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Days.Day10 (runDay) where

import Control.Lens hiding (uncons)
import Control.Monad
import Data.Array.IArray
import Data.Distributive
import Data.Fix
import Data.Function
import Data.Functor
import Data.Functor.Contravariant
import Data.Functor.Adjunction
import Data.Functor.Rep
import Data.List
import Data.Maybe
import Data.Monoid
import GHC.Num.Natural (naturalFromWord, naturalToWord)
import GHC.TypeNats
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text (Parser, takeText)
import qualified Data.Text as T
import Control.Monad.Representable.Reader (distributeRep, collectRep)
import Control.Arrow

------------ TYPES ------------
data MazeTile where
  Ground :: MazeTile -- .
  Start :: MazeTile  -- S
  NS :: MazeTile     -- |
  EW :: MazeTile     -- -
  NE :: MazeTile     -- L
  NW :: MazeTile     -- J
  SE :: MazeTile     -- F
  SW :: MazeTile     -- 7
  deriving (Bounded, Enum, Eq, Ord, Show)
makePrisms ''MazeTile

data Direction where
  Stop :: Direction
  North :: Direction
  East :: Direction
  South :: Direction
  West :: Direction
  deriving (Bounded, Enum, Eq, Ord, Show)
makePrisms ''Direction

knownNatToWord :: forall n. (KnownNat n) => Word
knownNatToWord = naturalToWord $ fromSNat @n natSing

knownNatToWord' :: forall n. (KnownNat n) => Word
knownNatToWord' = naturalToWord $ fromSNat @n natSing - 1

class (KnownNat len, KnownNat wid) => D2Bound len wid where
  d2Length :: Word
  d2Length = knownNatToWord @len
  d2Width :: Word
  d2Width = knownNatToWord @wid
  northBound :: Word
  northBound = 0
  eastBound :: Word
  eastBound = knownNatToWord' @wid
  southBound :: Word
  southBound = knownNatToWord' @len
  westBound :: Word
  westBound = 0
  d2Bounds :: ((Word, Word), (Word, Word))
  d2Bounds = ((0, 0), (knownNatToWord' @len, knownNatToWord' @wid))

instance (KnownNat len, KnownNat wid) => D2Bound len wid

data CoordF (len :: Nat) (wid :: Nat) a where
  CoordF :: {
    _y :: Word,
    _x :: Word,
    _z :: a
  } -> CoordF len wid a
  deriving (Eq, Functor, Ord, Show)
makeLenses ''CoordF

type Coord len wid = CoordF len wid ()
type Step len wid = CoordF len wid Direction

newtype MazeF (len :: Nat) (wid :: Nat) a =
  MazeF { unMazeF :: Array (Word, Word) a }
  deriving (Functor)
makeLenses ''MazeF

instance (D2Bound len wid) => Show (MazeF len wid a) where
  show _ = "maze"

data MazeStart a b where
  MazeStart :: forall len wid a b. (D2Bound len wid) => {
    maze :: MazeF len wid a,
    start :: CoordF len wid b
  } -> MazeStart a b

instance Show (MazeStart a b) where
  show _ = "maze start"

type Maze len wid = MazeF len wid MazeTile
type MazeStart' = MazeStart MazeTile Direction

instance (D2Bound len wid) =>
  Adjunction (CoordF len wid) (MazeF len wid) where
  unit _z = MazeF $ genArray (d2Bounds @len @wid) $ \(_x, _y) -> CoordF {..}
  counit CoordF {..} = unMazeF _z ! (_x, _y)
  leftAdjunct f _z = MazeF $ genArray (d2Bounds @len @wid) $ \(_x, _y) -> f CoordF {..}
  rightAdjunct f CoordF {..} = unMazeF (f _z) ! (_x, _y)

instance (D2Bound len wid) =>
  Representable (MazeF len wid) where
  type Rep (MazeF len wid) = CoordF len wid ()
  tabulate = tabulateAdjunction
  index = indexAdjunction

instance (D2Bound len wid) =>
  Distributive (MazeF len wid) where
  distribute = distributeRep
  collect = collectRep

type Input = MazeStart'

type OutputA = Word

type OutputB = Word

------------ PARSER ------------
toTile :: Char -> MazeTile
toTile '.' = Ground
toTile 'S' = Start
toTile '|' = NS
toTile '-' = EW
toTile 'L' = NE
toTile 'J' = NW
toTile 'F' = SE
toTile '7' = SW

fromTile :: MazeTile -> Char
fromTile Ground = '.'
fromTile Start = 'S'
fromTile NS = '|'
fromTile EW = '-'
fromTile NE = 'L'
fromTile NW = 'J'
fromTile SE = 'F'
fromTile SW = '7'

turn :: MazeTile -> Direction -> Direction
turn NS North = North
turn NS South = South
turn EW East = East
turn EW West = West
turn NE South = East
turn NE West = North
turn NW South = West
turn NW East = North
turn SE North = East
turn SE West = South
turn SW North = West
turn SW East = South
turn _ _ = Stop

nextStep :: forall len wid. (D2Bound len wid) =>
  Step len wid -> Step len wid
nextStep coord@CoordF {..} = case _z of
  Stop -> coord
  North -> if _y == northBound @len @wid
    then stop
    else coord { _y = pred _y }
  East -> if _x >= eastBound @len @wid
    then stop
    else coord { _x = succ _x }
  South -> if _y >= southBound @len @wid
    then stop
    else coord { _y = succ _y }
  West -> if _x == westBound @len @wid
    then stop
    else coord { _x = pred _x }
  where
    stop = coord { _z = Stop }

peek :: forall len wid. (D2Bound len wid) =>
  Maze len wid -> Step len wid -> MazeTile
peek maze coord@CoordF {..} = case _z of
  Stop -> zap coord
  North -> if _y == northBound @len @wid
    then Ground
    else zap coord { _y = pred _y }
  East -> if _x >= eastBound @len @wid
    then Ground
    else zap coord { _x = succ _x }
  South -> if _y >= southBound @len @wid
    then Ground
    else zap coord { _y = succ _y }
  West -> if _x == westBound @len @wid
    then Ground
    else zap coord { _x = pred _x }
  where
    zap = zapWithAdjunction const maze

findStart :: MonadFail m => T.Text -> m Word
findStart = maybe (fail "Could not find start.") (return . fromIntegral)
  . T.findIndex (== 'S')

findWidth :: MonadFail m => T.Text -> m Word
findWidth = maybe (fail "Could not find newline.") (return . fromIntegral)
  . T.findIndex (== '\n')

findAtCoord :: forall len wid a. (D2Bound len wid) =>
  T.Text -> CoordF len wid a -> MazeTile
findAtCoord text CoordF {..} = toTile $ T.index text $ fromIntegral
  $ succ (d2Width @len @wid) * _y + _x

findFirstStep :: forall {len :: Nat} {wid :: Nat}. (D2Bound len wid) =>
  Maze len wid -> Step len wid -> Direction
findFirstStep maze coord = fromMaybe Stop . find (/= Stop)
  . fmap (peek maze . (coord $>) >>= turn) $ enumFrom minBound

parseMaze :: Parser MazeStart'
parseMaze = do
  text <- takeText
  i <- findStart text
  w <- findWidth text
  let
    l = div (fromIntegral $ T.length text) (w + 1)
    (_y, _x) = divMod i (w + 1)
    len = naturalFromWord l
    wid = naturalFromWord w
  withSomeSNat len $ \(SNat @len) ->
    withSomeSNat wid $ \(SNat @wid) ->
      let
        _z = findFirstStep maze start
        maze = tabulate $ findAtCoord text
        start = CoordF {..}
      in
        return $ MazeStart @len @wid maze start

inputParser :: Parser Input
inputParser = parseMaze

runDay :: R.Day
runDay = R.runDay parseMaze partA partB

------------ PART A ------------
stepAndTurn :: (D2Bound len wid) =>
  Maze len wid -> Step len wid -> Step len wid
stepAndTurn maze step = n $> zapWithAdjunction turn maze n
  where
    n = nextStep step

mazePath :: (D2Bound len wid) =>
  Maze len wid -> Step len wid -> [Step len wid]
mazePath maze = unfoldr $ \step@CoordF{..} -> if _z == Stop
  then Nothing
  else Just (step, stepAndTurn maze step)

partA :: Input -> OutputA
partA MazeStart { maze, start } =
  flip div 2 $ fromIntegral $ length $ mazePath maze start

------------ PART B ------------
areHorizontallyContinuousTiles :: MazeTile -> MazeTile -> Bool
areHorizontallyContinuousTiles a b =
  turn a West /= Stop && turn b East /= Stop

isHorizontallyContinuous :: (D2Bound len wid) =>
  CoordF len wid MazeTile -> CoordF len wid MazeTile -> Bool
isHorizontallyContinuous a b =
  _y a == _y b && if
    | _x a == pred (_x b) ->
      areHorizontallyContinuousTiles (_z a) (_z b)
    | _x b == pred (_x a) ->
      areHorizontallyContinuousTiles (_z b) (_z a)
    | otherwise -> False

partitionLatitudes :: (D2Bound len wid) =>
  [CoordF len wid a] -> [[CoordF len wid a]]
partitionLatitudes = groupBy (on (==) _y) . sortBy (getComparison $
  contramap _y defaultComparison <> contramap _x defaultComparison)

groupLatitude :: (D2Bound len wid) =>
  [CoordF len wid MazeTile] -> [[CoordF len wid MazeTile]]
groupLatitude = foldr (\a as -> case Data.List.uncons as of
  Nothing -> [[a]]
  Just (b, bs) -> if isHorizontallyContinuous a (head b)
    then (a:b):bs
    else [a]:as) []

isTangent :: (D2Bound len wid) =>
  [CoordF len wid MazeTile] -> Bool
isTangent = maybe False (\(a, b) -> turn a West == turn b East)
  <<< fmap (each %~ _z) . traverse (fmap snd . unsnoc)
  <=< uncons

-- List hylomorphism
hylo' :: (Maybe (c, b) -> b) -> (a -> Maybe (c, a)) -> a -> b
hylo' alg coalg = go
  where
    go = alg . (fmap . second) go . coalg

-- List paramorphism
para' :: (Maybe (a, ([a], b)) -> b) -> [a] -> b
para' alg = go
  where
    go = alg . (fmap . second) (id &&& go) . uncons

-- Take one extra
takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p = foldr (\x xs -> if p x then x:xs else [x]) []

-- Drop one extra
dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 p = para' $ \case
  Nothing -> []
  Just (x, (xs, ys)) -> if p x then ys else xs

takeDropWhile1 :: (a -> Bool) -> [a] -> ([a], [a])
takeDropWhile1 p = takeWhile1 p &&& dropWhile1 p

scanLatitude :: (D2Bound len wid) =>
  [[CoordF len wid MazeTile]] -> Word
scanLatitude = hylo' alg coalg
  where
    alg :: (D2Bound len wid) =>
      Maybe ([[CoordF len wid MazeTile]], Word) -> Word
    alg = \case
      Nothing -> 0
      Just (segments, totalLength) -> gapLengths segments + totalLength
    gapLengths = sum <<<
      zipWith (\a b -> (_x . head $ b) - (_x . last $ a) - 1) <*> tail
    coalg :: (D2Bound len wid) =>
      [[CoordF len wid MazeTile]] ->
        Maybe ([[CoordF len wid MazeTile]], [[CoordF len wid MazeTile]])
    coalg = fmap (uncurry (first . (:)) . second (takeDropWhile1 isTangent))
      . uncons . dropWhile isTangent

coordWithTile :: (D2Bound len wid) =>
  Maze len wid -> CoordF len wid a -> CoordF len wid MazeTile
coordWithTile maze = ($>) <*> indexAdjunction maze

pathWithTiles :: (D2Bound len wid) =>
  Maze len wid -> Step len wid -> [CoordF len wid MazeTile]
pathWithTiles maze = fmap (coordWithTile maze) . mazePath maze

partB :: Input -> OutputB
partB MazeStart { maze, start } =
  auf (_Wrapping Sum) (foldMapOf traverse) (scanLatitude . groupLatitude)
    . partitionLatitudes $ pathWithTiles maze start
