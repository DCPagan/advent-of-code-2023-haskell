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

type I = Word

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

data CoordF' (len :: Nat) (wid :: Nat) a where
  CoordF' :: {
    _x' :: Word,
    _y' :: Word,
    _z' :: a
  } -> CoordF' (len :: Nat) (wid :: Nat) a
  deriving (Eq, Show, Functor)
makeLenses ''CoordF'

type Coord = CoordF ()
type Step = CoordF Direction

type Coord' len wid = CoordF' len wid ()
type Step' len wid = CoordF' len wid Direction

-- Dependent product
newtype MazeF a = MazeF { unMazeF :: I -> I -> Array (I, I) a }
  deriving (Functor)
makeLenses ''MazeF

instance Show (MazeF a) where
  show _ = "maze"

newtype MazeF' (len :: Nat) (wid :: Nat) a =
  MazeF' { unMazeF' :: Array (Word, Word) a }
  deriving (Functor)
makeLenses ''MazeF'

instance (KnownNat len, KnownNat wid) => Show (MazeF' len wid a) where
  show _ = "maze"

data MazeStart a b where
  MazeStart :: forall len wid a b. (KnownNat len, KnownNat wid) => {
    maze :: MazeF' len wid a,
    start :: CoordF' len wid b
  } -> MazeStart a b

instance Show (MazeStart a b) where
  show _ = "maze start"

type Maze = MazeF MazeTile

type Maze' len wid = MazeF' len wid MazeTile
type MazeStart' = MazeStart MazeTile Direction

knownNatToWord :: forall n. (KnownNat n) => Word
knownNatToWord = naturalToWord $ fromSNat @n natSing

knownNatToWord' :: forall n. (KnownNat n) => Word
knownNatToWord' = naturalToWord $ fromSNat @n natSing - 1

instance Adjunction CoordF MazeF where
  unit _z = MazeF $
    \_len _wid -> genArray ((1, 1), (_len, _wid)) $
    \(_x, _y) -> CoordF {..}
  counit CoordF {..} = unMazeF _z _len _wid ! (_x, _y)
  leftAdjunct f _z = MazeF $
    \_len _wid -> genArray ((1, 1), (_len, _wid)) $
    \(_x, _y) -> f CoordF {..}
  rightAdjunct f CoordF {..} = unMazeF (f _z) _len _wid ! (_x, _y)

instance Representable MazeF where
  type Rep MazeF = Coord
  tabulate = tabulateAdjunction
  index = indexAdjunction

instance Distributive MazeF where
  distribute = distributeRep
  collect = collectRep

instance forall len wid. (KnownNat len, KnownNat wid) =>
  Adjunction (CoordF' len wid) (MazeF' len wid) where
  unit _z' = MazeF' $
    genArray ((0, 0), (knownNatToWord' @len, knownNatToWord' @wid)) $
    \(_x', _y') -> CoordF' {..}
  counit CoordF' {..} = unMazeF' @len @wid _z' ! (_x', _y')
  leftAdjunct f _z' = MazeF' $
    genArray ((0, 0), (knownNatToWord' @len, knownNatToWord' @len)) $
    \(_x', _y') -> f CoordF' {..}
  rightAdjunct f CoordF' {..} = unMazeF' @len @wid (f _z') ! (_x', _y')

instance forall len wid. (KnownNat len, KnownNat wid) =>
  Representable (MazeF' len wid) where
  type Rep (MazeF' len wid) = CoordF' len wid ()
  tabulate = tabulateAdjunction
  index = indexAdjunction

instance forall len wid. (KnownNat len, KnownNat wid) =>
  Distributive (MazeF' len wid) where
  distribute = distributeRep
  collect = collectRep

type Input = (Maze, Step)
type Input' = (Natural, Natural, MazeStart')

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

nextStep :: Step -> Step
nextStep coord@CoordF {..}
  | _z == Stop = coord
  | _z == North = if _y == 0
    then stop
    else coord { _y = pred _y }
  | _z == East = if _x >= _wid - 1
    then stop
    else coord { _x = succ _x }
  | _z == South = if _y >= _len - 1
    then stop
    else coord { _y = succ _y }
  | _z == West = if _x == 0
    then stop
    else coord { _x = pred _x }
  where
    stop = coord { _z = Stop }

nextStep' :: forall len wid. (KnownNat len, KnownNat wid) =>
  Step' len wid -> Step' len wid
nextStep' coord@CoordF' {..}
  | _z' == Stop = coord
  | _z' == North = if _y' == 0
    then stop
    else coord { _y' = pred _y' }
  | _z' == East = if _x' >= wid
    then stop
    else coord { _x' = succ _x' }
  | _z' == South = if _y' >= len
    then stop
    else coord { _y' = succ _y' }
  | _z' == West = if _x' == 0
    then stop
    else coord { _x' = pred _x' }
  where
    stop = coord { _z' = Stop }
    len = knownNatToWord' @len
    wid = knownNatToWord' @wid

peek :: Maze -> Step -> MazeTile
peek maze coord@CoordF { _z = Stop } = zapWithAdjunction const maze coord
peek maze coord@CoordF { _z = North, _y }
  | _y == 0 = Ground
  | otherwise = zapWithAdjunction const maze coord { _y = pred _y }
peek maze coord@CoordF { _z = East, _x, _wid }
  | _x >= _wid - 1 = Ground
  | otherwise = zapWithAdjunction const maze coord { _x = succ _x }
peek maze coord@CoordF { _z = South, _y, _len }
  | _y >= _len - 1 = Ground
  | otherwise = zapWithAdjunction const maze coord { _y = succ _y }
peek maze coord@CoordF { _z = West, _x }
  | _x == 0 = Ground
  | otherwise = zapWithAdjunction const maze coord { _x = pred _x }

peek' :: forall len wid. (KnownNat len, KnownNat wid) =>
  Maze' len wid -> Step' len wid -> MazeTile
peek' maze coord@CoordF' { _z' = Stop } = zapWithAdjunction const maze coord
peek' maze coord@CoordF' { _z' = North, _y' }
  | _y' == 0 = Ground
  | otherwise = zapWithAdjunction const maze coord { _y' = pred _y' }
peek' maze coord@CoordF' { _z' = East, _x' }
  | _x' >= knownNatToWord' @wid = Ground
  | otherwise = zapWithAdjunction const maze coord { _x' = succ _x' }
peek' maze coord@CoordF' { _z' = South, _y' }
  | _y' >= knownNatToWord' @len = Ground
  | otherwise = zapWithAdjunction const maze coord { _y' = succ _y' }
peek' maze coord@CoordF' { _z' = West, _x' }
  | _x' == 0 = Ground
  | otherwise = zapWithAdjunction const maze coord { _x' = pred _x' }

findStart :: MonadFail m => T.Text -> m I
findStart text = maybe (fail "Could not find start.") (return . fromIntegral)
  $ T.findIndex (== 'S') text

findWidth :: MonadFail m => T.Text -> m I
findWidth text = maybe (fail "Could not find newline.") (return . fromIntegral)
  $ T.findIndex (== '\n') text

findAtCoord :: T.Text -> CoordF a -> MazeTile
findAtCoord text CoordF {..} = toTile $ T.index text $ fromIntegral
  $ succ _wid * _y + _x

findAtCoord' :: forall len wid a. (KnownNat len, KnownNat wid) =>
  T.Text -> CoordF' len wid a -> MazeTile
findAtCoord' text CoordF' {..} = toTile $ T.index text $ fromIntegral
  $ succ (knownNatToWord @wid) * _y' + _x'

findFirstStep :: Maze -> Step -> Direction
findFirstStep maze coord = fromMaybe Stop . find (/= Stop)
  . fmap (peek maze . (coord $>) >>= turn) $ enumFrom minBound

findFirstStep' :: (KnownNat len, KnownNat wid) =>
  Maze' len wid -> Step' len wid -> Direction
findFirstStep' maze coord = fromMaybe Stop . find (/= Stop)
  . fmap (peek' maze . (coord $>) >>= turn) $ enumFrom minBound

parseMaze :: Parser (Maze, Step)
parseMaze = do
  text <- takeText
  i <- findStart text
  _wid <- findWidth text
  let
    _len = div (fromIntegral $ T.length text) (_wid + 1)
    _x = rem i (_wid + 1)
    _y = div i (_wid + 1)
    _z = findFirstStep maze start
    maze = tabulate $ findAtCoord text
    start = CoordF {..}
  return (maze, start)

parseMaze' :: Parser Input'
parseMaze' = do
  text <- takeText
  i <- findStart text
  w <- findWidth text
  let
    l = div (fromIntegral $ T.length text) (w + 1)
    _x' = rem i (w + 1)
    _y' = div i (w + 1)
    len = naturalFromWord l
    wid = naturalFromWord w
  withSomeSNat len $ \case
    SNat @len -> withSomeSNat wid $ \case
      SNat @wid ->
        let
          _z' = findFirstStep' @len @wid maze start
          maze = tabulate $ findAtCoord' @len @wid text
          start = CoordF' {..}
        in
          return (len, wid, MazeStart maze start)

inputParser :: Parser Input
inputParser = parseMaze

inputParser' :: Parser Input'
inputParser' = parseMaze'

runDay :: R.Day
runDay = R.runDay parseMaze' partA' partB'

------------ PART A ------------
stepAndTurn :: Maze -> Step -> Step
stepAndTurn maze step = n $> zapWithAdjunction turn maze n
  where
    n = nextStep step

stepAndTurn' :: (KnownNat len, KnownNat wid) =>
  Maze' len wid -> Step' len wid -> Step' len wid
stepAndTurn' maze step = n $> zapWithAdjunction turn maze n
  where
    n = nextStep' step

mazePath :: Maze -> Step -> [Step]
mazePath maze = unfoldr $ \step@CoordF{..} -> if _z == Stop
  then Nothing
  else Just (step, stepAndTurn maze step)

mazePath' :: (KnownNat len, KnownNat wid) =>
  Maze' len wid -> Step' len wid -> [Step' len wid]
mazePath' maze = unfoldr $ \step@CoordF'{..} -> if _z' == Stop
  then Nothing
  else Just (step, stepAndTurn' maze step)

partA :: Input -> OutputA
partA = flip div 2 . fromIntegral . length . uncurry mazePath

partA' :: Input' -> OutputA
partA' (len, wid, mazeStart) = withSomeSNat len $ \case
  SNat @len -> withSomeSNat wid $ \case
    SNat @wid -> case mazeStart of
      MazeStart maze start ->
        flip div 2 $ fromIntegral $ length $ mazePath' maze start

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

isHorizontallyContinuous' :: (KnownNat len, KnownNat wid) =>
  CoordF' len wid MazeTile -> CoordF' len wid MazeTile -> Bool
isHorizontallyContinuous' a b =
  _y' a == _y' b && if
    | _x' a == pred (_x' b) ->
      areHorizontallyContinuousTiles (_z' a) (_z' b)
    | _x' b == pred (_x' a) ->
      areHorizontallyContinuousTiles (_z' b) (_z' a)
    | otherwise -> False

isVerticallyContinuous :: CoordF MazeTile -> CoordF MazeTile -> Bool
isVerticallyContinuous a b =
  _x a == _x b && if
    | _y a == pred (_y b) ->
      turn (_z a) North /= Stop && turn (_z b) South /= Stop
    | _y b == pred (_y a) ->
      turn (_z b) North /= Stop && turn (_z a) South /= Stop
    | otherwise -> False

isVerticallyContinuous' :: (KnownNat len, KnownNat wid) =>
  CoordF' len wid MazeTile -> CoordF' len wid MazeTile -> Bool
isVerticallyContinuous' a b =
  _x' a == _x' b && if
    | _y' a == pred (_y' b) ->
      turn (_z' a) North /= Stop && turn (_z' b) South /= Stop
    | _y' b == pred (_y' a) ->
      turn (_z' b) North /= Stop && turn (_z' a) South /= Stop
    | otherwise -> False

isContinuous :: CoordF MazeTile -> CoordF MazeTile -> Bool
isContinuous a b =
  isHorizontallyContinuous a b && isVerticallyContinuous a b

isContinuous' :: (KnownNat len, KnownNat wid) =>
  CoordF' len wid MazeTile -> CoordF' len wid MazeTile -> Bool
isContinuous' a b =
  isHorizontallyContinuous' a b && isVerticallyContinuous' a b

partitionLatitudes :: [CoordF a] -> [[CoordF a]]
partitionLatitudes =
  groupBy (on (==) _y) . sortBy (getComparison (
    contramap _y defaultComparison <> contramap _x defaultComparison))

partitionLatitudes' :: (KnownNat len, KnownNat wid) =>
  [CoordF' len wid a] -> [[CoordF' len wid a]]
partitionLatitudes' =
  groupBy (on (==) _y') . sortBy (getComparison (
    contramap _y' defaultComparison <> contramap _x' defaultComparison))

groupLatitude :: [CoordF MazeTile] -> [[CoordF MazeTile]]
groupLatitude = foldr (\a as -> case Data.List.uncons as of
  Nothing -> [[a]]
  Just (b, bs) -> if isHorizontallyContinuous a (head b)
    then (a:b):bs
    else [a]:as) []

groupLatitude' :: (KnownNat len, KnownNat wid) =>
  [CoordF' len wid MazeTile] -> [[CoordF' len wid MazeTile]]
groupLatitude' = foldr (\a as -> case Data.List.uncons as of
  Nothing -> [[a]]
  Just (b, bs) -> if isHorizontallyContinuous' a (head b)
    then (a:b):bs
    else [a]:as) []

isTangent :: [CoordF MazeTile] -> Bool
isTangent [] = False
isTangent [_] = False
isTangent l = turn (_z . head $ l) West == turn (_z . last $ l) East

isTangent' :: (KnownNat len, KnownNat wid) =>
  [CoordF' len wid MazeTile] -> Bool
isTangent' [] = False
isTangent' [_] = False
isTangent' l = turn (_z' . head $ l) West == turn (_z' . last $ l) East

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

latitudeArea :: [[CoordF MazeTile]] -> Word
latitudeArea = hylo' alg coalg
  where
    alg :: Maybe ([[CoordF MazeTile]], Word) -> Word
    alg = \case
      Nothing -> 0
      Just (segments, totalLength) -> gapLengths segments + totalLength
    gapLengths = sum <<<
      zipWith (\a b -> (_x . head $ b) - (_x . last $ a) - 1) <*> tail
    coalg :: [[CoordF MazeTile]] -> Maybe ([[CoordF MazeTile]], [[CoordF MazeTile]])
    coalg = fmap (uncurry (first . (:)) . second (takeDropWhile1 isTangent))
      . uncons . dropWhile isTangent

latitudeArea' :: (KnownNat len, KnownNat wid) =>
  [[CoordF' len wid MazeTile]] -> Word
latitudeArea' = hylo' alg coalg
  where
    alg :: (KnownNat len, KnownNat wid) =>
      Maybe ([[CoordF' len wid MazeTile]], Word) -> Word
    alg = \case
      Nothing -> 0
      Just (segments, totalLength) -> gapLengths segments + totalLength
    gapLengths = sum <<<
      zipWith (\a b -> (_x' . head $ b) - (_x' . last $ a) - 1) <*> tail
    coalg :: (KnownNat len, KnownNat wid) =>
      [[CoordF' len wid MazeTile]] ->
        Maybe ([[CoordF' len wid MazeTile]], [[CoordF' len wid MazeTile]])
    coalg = fmap (uncurry (first . (:)) . second (takeDropWhile1 isTangent'))
      . uncons . dropWhile isTangent'

coordWithTile :: Maze -> CoordF a -> CoordF MazeTile
coordWithTile maze = ($>) <*> indexAdjunction maze

coordWithTile' :: (KnownNat len, KnownNat wid) =>
  Maze' len wid -> CoordF' len wid a -> CoordF' len wid MazeTile
coordWithTile' maze = ($>) <*> indexAdjunction maze

pathWithTiles :: Maze -> Step -> [CoordF MazeTile]
pathWithTiles maze = fmap (coordWithTile maze) . mazePath maze

pathWithTiles' :: (KnownNat len, KnownNat wid) =>
  Maze' len wid -> Step' len wid -> [CoordF' len wid MazeTile]
pathWithTiles' maze = fmap (coordWithTile' maze) . mazePath' maze

partB :: Input -> OutputB
partB = auf (_Wrapping Sum) (foldMapOf traverse) (latitudeArea . groupLatitude)
  . partitionLatitudes . uncurry pathWithTiles

partB' :: Input' -> OutputB
partB' (len, wid, mazeStart) = withSomeSNat len $ \case
  SNat @len -> withSomeSNat wid $ \case
    SNat @wid -> case mazeStart of
      MazeStart maze start ->
        auf (_Wrapping Sum) (foldMapOf traverse) (latitudeArea' . groupLatitude')
          . partitionLatitudes' $ pathWithTiles' maze start
