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
  deriving (Bounded, Enum, Eq, Show)
makePrisms ''MazeTile

data Direction where
  Stop :: Direction
  North :: Direction
  East :: Direction
  South :: Direction
  West :: Direction
  deriving (Bounded, Enum, Eq, Show)
makePrisms ''Direction

data CoordF (len :: Nat) (wid :: Nat) a where
  CoordF :: {
    _x :: Word,
    _y :: Word,
    _z :: a
  } -> CoordF (len :: Nat) (wid :: Nat) a
  deriving (Eq, Show, Functor)
makeLenses ''CoordF

type Coord len wid = CoordF len wid ()
type Step len wid = CoordF len wid Direction

newtype MazeF (len :: Nat) (wid :: Nat) a =
  MazeF { unMazeF :: Array (Word, Word) a }
  deriving (Functor)
makeLenses ''MazeF

instance (KnownNat len, KnownNat wid) => Show (MazeF len wid a) where
  show _ = "maze"

data MazeStart a b where
  MazeStart :: forall len wid a b. (KnownNat len, KnownNat wid) => {
    maze :: MazeF len wid a,
    start :: CoordF len wid b
  } -> MazeStart a b

instance Show (MazeStart a b) where
  show _ = "maze start"

type Maze len wid = MazeF len wid MazeTile
type MazeStart' = MazeStart MazeTile Direction

knownNatToWord :: forall n. (KnownNat n) => Word
knownNatToWord = naturalToWord $ fromSNat @n natSing

knownNatToWord' :: forall n. (KnownNat n) => Word
knownNatToWord' = naturalToWord $ fromSNat @n natSing - 1

instance (KnownNat len, KnownNat wid) =>
  Adjunction (CoordF len wid) (MazeF len wid) where
  unit _z = MazeF $
    genArray ((0, 0), (knownNatToWord' @len, knownNatToWord' @wid)) $
    \(_x, _y) -> CoordF {..}
  counit CoordF {..} = unMazeF _z ! (_x, _y)
  leftAdjunct f _z = MazeF $
    genArray ((0, 0), (knownNatToWord' @len, knownNatToWord' @len)) $
    \(_x, _y) -> f CoordF {..}
  rightAdjunct f CoordF {..} = unMazeF (f _z) ! (_x, _y)

instance (KnownNat len, KnownNat wid) =>
  Representable (MazeF len wid) where
  type Rep (MazeF len wid) = CoordF len wid ()
  tabulate = tabulateAdjunction
  index = indexAdjunction

instance (KnownNat len, KnownNat wid) =>
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

nextStep :: forall len wid. (KnownNat len, KnownNat wid) =>
  Step len wid -> Step len wid
nextStep coord@CoordF {..}
  | _z == Stop = coord
  | _z == North = if _y == 0
    then stop
    else coord { _y = pred _y }
  | _z == East = if _x >= wid
    then stop
    else coord { _x = succ _x }
  | _z == South = if _y >= len
    then stop
    else coord { _y = succ _y }
  | _z == West = if _x == 0
    then stop
    else coord { _x = pred _x }
  where
    stop = coord { _z = Stop }
    len = knownNatToWord' @len
    wid = knownNatToWord' @wid

peek :: forall len wid. (KnownNat len, KnownNat wid) =>
  Maze len wid -> Step len wid -> MazeTile
peek maze coord@CoordF {..}
  | _z == Stop = zap coord
  | _z == North = if _y == 0
    then Ground
    else zap coord { _y = pred _y }
  | _z == East = if _x >= wid
    then Ground
    else zap coord { _x = succ _x }
  | _z == South = if _y >= len
    then Ground
    else zap coord { _y = succ _y }
  | _z == West = if _x == 0
    then Ground
    else zap coord { _x = pred _x }
  where
    zap = zapWithAdjunction const maze
    len = knownNatToWord' @len
    wid = knownNatToWord' @wid

findStart :: MonadFail m => T.Text -> m Word
findStart = maybe (fail "Could not find start.") (return . fromIntegral)
  . T.findIndex (== 'S')

findWidth :: MonadFail m => T.Text -> m Word
findWidth = maybe (fail "Could not find newline.") (return . fromIntegral)
  . T.findIndex (== '\n')

findAtCoord :: forall len wid a. (KnownNat len, KnownNat wid) =>
  T.Text -> CoordF len wid a -> MazeTile
findAtCoord text CoordF {..} = toTile $ T.index text $ fromIntegral
  $ succ (knownNatToWord @wid) * _y + _x

findFirstStep :: (KnownNat len, KnownNat wid) =>
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
stepAndTurn :: (KnownNat len, KnownNat wid) =>
  Maze len wid -> Step len wid -> Step len wid
stepAndTurn maze step = n $> zapWithAdjunction turn maze n
  where
    n = nextStep step

mazePath :: (KnownNat len, KnownNat wid) =>
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

isHorizontallyContinuous :: (KnownNat len, KnownNat wid) =>
  CoordF len wid MazeTile -> CoordF len wid MazeTile -> Bool
isHorizontallyContinuous a b =
  _y a == _y b && if
    | _x a == pred (_x b) ->
      areHorizontallyContinuousTiles (_z a) (_z b)
    | _x b == pred (_x a) ->
      areHorizontallyContinuousTiles (_z b) (_z a)
    | otherwise -> False

partitionLatitudes :: (KnownNat len, KnownNat wid) =>
  [CoordF len wid a] -> [[CoordF len wid a]]
partitionLatitudes =
  groupBy (on (==) _y) . sortBy (getComparison (
    contramap _y defaultComparison <> contramap _x defaultComparison))

groupLatitude :: (KnownNat len, KnownNat wid) =>
  [CoordF len wid MazeTile] -> [[CoordF len wid MazeTile]]
groupLatitude = foldr (\a as -> case Data.List.uncons as of
  Nothing -> [[a]]
  Just (b, bs) -> if isHorizontallyContinuous a (head b)
    then (a:b):bs
    else [a]:as) []

isTangent :: (KnownNat len, KnownNat wid) =>
  [CoordF len wid MazeTile] -> Bool
isTangent [] = False
isTangent [_] = False
isTangent l = turn (_z . head $ l) West == turn (_z . last $ l) East

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

latitudeArea :: (KnownNat len, KnownNat wid) =>
  [[CoordF len wid MazeTile]] -> Word
latitudeArea = hylo' alg coalg
  where
    alg :: (KnownNat len, KnownNat wid) =>
      Maybe ([[CoordF len wid MazeTile]], Word) -> Word
    alg = \case
      Nothing -> 0
      Just (segments, totalLength) -> gapLengths segments + totalLength
    gapLengths = sum <<<
      zipWith (\a b -> (_x . head $ b) - (_x . last $ a) - 1) <*> tail
    coalg :: (KnownNat len, KnownNat wid) =>
      [[CoordF len wid MazeTile]] ->
        Maybe ([[CoordF len wid MazeTile]], [[CoordF len wid MazeTile]])
    coalg = fmap (uncurry (first . (:)) . second (takeDropWhile1 isTangent))
      . uncons . dropWhile isTangent

coordWithTile :: (KnownNat len, KnownNat wid) =>
  Maze len wid -> CoordF len wid a -> CoordF len wid MazeTile
coordWithTile maze = ($>) <*> indexAdjunction maze

pathWithTiles :: (KnownNat len, KnownNat wid) =>
  Maze len wid -> Step len wid -> [CoordF len wid MazeTile]
pathWithTiles maze = fmap (coordWithTile maze) . mazePath maze

partB :: Input -> OutputB
partB MazeStart { maze, start } =
  auf (_Wrapping Sum) (foldMapOf traverse) (latitudeArea . groupLatitude)
    . partitionLatitudes $ pathWithTiles maze start
