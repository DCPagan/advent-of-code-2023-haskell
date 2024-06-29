{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
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

type I = Int

-- Dependent sum
data CoordF a where
  CoordF :: {
    _len :: I,
    _wid :: I,
    _x :: I,
    _y :: I,
    _z :: a
  } -> CoordF a
  deriving (Eq, Show, Functor)
makeLenses ''CoordF

type Coord = CoordF ()
type Step = CoordF Direction

-- Dependent product
newtype MazeF a = MazeF { unMazeF :: I -> I -> Array (I, I) a }
  deriving (Functor)
makeLenses ''MazeF

instance Show (MazeF a) where
  show _ = "maze"

type Maze = MazeF MazeTile

instance Adjunction CoordF MazeF where
  unit _z = MazeF $
    \_len _wid -> genArray ((1, 1), (_len, _wid)) $ \(_x, _y) -> CoordF {..}
  counit CoordF {..} = unMazeF _z _len _wid ! (_x, _y)
  leftAdjunct f _z = MazeF $
    \_len _wid -> genArray ((1, 1), (_len, _wid)) $ \(_x, _y) -> f CoordF {..}
  rightAdjunct f CoordF {..} = unMazeF (f _z) _len _wid ! (_x, _y)

instance Representable MazeF where
  type Rep MazeF = Coord
  tabulate = tabulateAdjunction
  index = indexAdjunction

instance Distributive MazeF where
  distribute = distributeRep
  collect = collectRep

type Input = (Maze, Step)

type OutputA = I

type OutputB = I

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

nextStep :: Step -> Step
nextStep coord@CoordF {..}
  | _z == Stop = coord
  | _z == North = if _y <= 1
    then stop
    else coord { _y = pred _y }
  | _z == East = if _x >= _wid
    then stop
    else coord { _x = succ _x }
  | _z == South = if _y >= _len
    then stop
    else coord { _y = succ _y }
  | _z == West = if _x <= 1
    then stop
    else coord { _x = pred _x }
  where
    stop = coord { _z = Stop }

peek :: Maze -> Step -> MazeTile
peek maze coord@CoordF { _z = Stop } = zapWithAdjunction const maze coord
peek maze coord@CoordF { _z = North, _y }
  | _y <= 1 = Ground
  | otherwise = zapWithAdjunction const maze coord { _y = pred _y }
peek maze coord@CoordF { _z = East, _x, _len }
  | _x >= _len = Ground
  | otherwise = zapWithAdjunction const maze coord { _x = min (succ _x) _len }
peek maze coord@CoordF { _z = South, _y, _len }
  | _y >= _len = Ground
  | otherwise = zapWithAdjunction const maze coord { _y = min (succ _y) _len }
peek maze coord@CoordF { _z = West, _x }
  | _x <= 1 = Ground
  | otherwise = zapWithAdjunction const maze coord { _x = max (pred _x) 1 }

findStart :: MonadFail m => T.Text -> m Int
findStart text = maybe (fail "Could not find start.") return
  $ T.findIndex (== 'S') text

findWidth :: MonadFail m => T.Text -> m I
findWidth text = maybe (fail "Could not find newline.") return
  $ T.findIndex (== '\n') text

findAtCoord :: T.Text -> CoordF a -> MazeTile
findAtCoord text CoordF {..} = toTile $ T.index text
  $ succ _wid * pred _y + pred _x

findFirstStep :: Maze -> Step -> Direction
findFirstStep maze coord = fromMaybe Stop . find (/= Stop)
  . fmap (peek maze . (coord $>) >>= turn) $ enumFrom minBound

parseMaze :: Parser (Maze, Step)
parseMaze = do
  text <- takeText
  i <- findStart text
  _wid <- findWidth text
  let
    _len = div (T.length text) (_wid + 1)
    _x = rem i (_wid + 1) + 1
    _y = div i (_wid + 1) + 1
    _z = findFirstStep maze start
    maze = tabulate $ findAtCoord text
    start = CoordF {..}
  return (maze, start)

inputParser :: Parser Input
inputParser = parseMaze

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PART A ------------
stepAndTurn :: Maze -> Step -> Step
stepAndTurn maze step = n $> zapWithAdjunction turn maze n
  where
    n = nextStep step

mazePath :: Maze -> Step -> [Step]
mazePath maze = unfoldr $ \step@CoordF{..} -> if _z == Stop
  then Nothing
  else Just (step, stepAndTurn maze step)

partA :: Input -> OutputA
partA = flip div 2 . length . uncurry mazePath

------------ PART B ------------
areHorizontallyContinuousTiles :: MazeTile -> MazeTile -> Bool
areHorizontallyContinuousTiles a b =
  turn a West /= Stop && turn b East /= Stop

isHorizontallyContinuous :: CoordF MazeTile -> CoordF MazeTile -> Bool
isHorizontallyContinuous a b =
  _y a == _y b && if
    | _x a == pred (_x b) ->
      areHorizontallyContinuousTiles (_z a) (_z b)
    | _x b == pred (_x a) ->
      areHorizontallyContinuousTiles (_z b) (_z a)
    | otherwise -> False

isVerticallyContinuous :: CoordF MazeTile -> CoordF MazeTile -> Bool
isVerticallyContinuous a b =
  _x a == _x b && if
    | _y a == pred (_y b) ->
      turn (_z a) North /= Stop && turn (_z b) South /= Stop
    | _y b == pred (_y a) ->
      turn (_z b) North /= Stop && turn (_z a) South /= Stop
    | otherwise -> False

isContinuous :: CoordF MazeTile -> CoordF MazeTile -> Bool
isContinuous a b =
  isHorizontallyContinuous a b && isVerticallyContinuous a b

partitionLatitudes :: [CoordF a] -> [[CoordF a]]
partitionLatitudes =
  groupBy (on (==) _y) . sortBy (getComparison (
    contramap _y defaultComparison <> contramap _x defaultComparison))

groupLatitude :: [CoordF MazeTile] -> [[CoordF MazeTile]]
groupLatitude = foldr (\a as -> case Data.List.uncons as of
  Nothing -> [[a]]
  Just (b, bs) -> if isHorizontallyContinuous a (head b)
    then (a:b):bs
    else [a]:as) []

isTangent :: [CoordF MazeTile] -> Bool
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

latitudeArea :: [[CoordF MazeTile]] -> Int
latitudeArea = hylo' alg coalg
  where
    alg :: Maybe ([[CoordF MazeTile]], Int) -> Int
    alg = \case
      Nothing -> 0
      Just (segments, totalLength) -> gapLengths segments + totalLength
    gapLengths = sum <<<
      zipWith (\a b -> (_x . head $ b) - (_x . last $ a) - 1) <*> tail
    coalg :: [[CoordF MazeTile]] -> Maybe ([[CoordF MazeTile]], [[CoordF MazeTile]])
    coalg [] = Nothing
    coalg (a:as) = if isTangent a
      then coalg as
      else Just $ first (a:) $ takeDropWhile1 isTangent as

coordWithTile :: Maze -> CoordF a -> CoordF MazeTile
coordWithTile maze = ($>) <*> indexAdjunction maze

pathWithTiles :: Maze -> Step -> [CoordF MazeTile]
pathWithTiles maze = fmap (coordWithTile maze) . mazePath maze

partB :: Input -> OutputB
partB = auf (_Wrapping Sum) (foldMapOf traverse) (latitudeArea . groupLatitude)
  . partitionLatitudes . uncurry pathWithTiles
