{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Days.Day16 (runDay) where

import Control.Arrow
import Control.Lens hiding (uncons)
import Control.Monad
import Control.Monad.Combinators
import Control.Monad.Primitive
import Control.Monad.Reader

import Data.Array.IArray
import Data.Array.MArray
import Data.Array.ST
import Data.Attoparsec.Text (Parser, anyChar, endOfLine)
import Data.Distributive
import Data.Function
import Data.Functor
import Data.Functor.Const
import Data.Functor.Adjunction
import Data.Functor.Rep
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Void
import GHC.Num.Natural (naturalFromWord, naturalToWord)
import GHC.TypeNats

import qualified Program.RunDay as R (Day,runDay)

import qualified Text.ParserCombinators.ReadP as R
import qualified Text.ParserCombinators.ReadPrec as RP
import Text.Read

import qualified Util.Util as U

------------ TYPES ------------
data Tile where
  Space :: Tile     -- .
  RMirror :: Tile   -- /
  LMirror :: Tile   -- \
  HSplitter :: Tile -- -
  VSplitter :: Tile -- |
  Error :: Tile     -- ¿
  deriving (Enum,Bounded,Eq,Ord)

makePrisms ''Tile

toTile :: Char -> Tile
toTile '.' = Space
toTile '/' = RMirror
toTile '\\' = LMirror
toTile '-' = HSplitter
toTile '|' = VSplitter
toTile _ = Error

fromTile :: Tile -> Char
fromTile Space = '.'
fromTile RMirror = '/'
fromTile LMirror = '\\'
fromTile HSplitter = '-'
fromTile VSplitter = '|'
fromTile Error = '¿'

instance Read Tile where
  readPrec = do
    x <- toTile <$> RP.get
    guard (Error /= x) $> x

  readListPrec = many readPrec

instance Show Tile where
  show = singleton . fromTile

  showList = (++) . (>>= show)

data Direction where
  North :: Direction
  East :: Direction
  South :: Direction
  West :: Direction
  deriving (Enum, Bounded, Eq, Ord, Read, Show)

makePrisms ''Direction

data Energized where
  Energized :: {
    _north :: Bool,
    _east :: Bool,
    _south :: Bool,
    _west :: Bool
  } -> Energized
  deriving (Eq, Ord)

makeLenses ''Energized

dark :: Energized
dark = Energized {
  _north = False,
  _east = False,
  _south = False,
  _west = False
}

visited :: Direction -> Lens' Energized Bool
visited North = north
visited South = south
visited East = east
visited West = west

_Dark :: Prism' Energized ()
_Dark = nearly dark (dark ==)

instance Semigroup Energized where
  a <> b = Energized {
    _north = _north a || _north b,
    _east = _east a || _east b,
    _south = _south a || _south b,
    _west = _west a || _west b
  }

instance Monoid Energized where
  mempty = dark

data TileHistory where
  TileHistory :: {
    _tile :: Tile,
    _energized :: Energized
  } -> TileHistory
  deriving (Eq, Ord)

makeLenses ''TileHistory

instance Show TileHistory where
  show TileHistory { .. } = show _tile

  showList ls = (intercalate "," (show <$> ls) ++)

type Coords = (Word, Word)

data CoordsF a where
  CoordsF :: {
    _y :: Word,
    _x :: Word,
    _z :: a
  } -> CoordsF a
  deriving (Eq, Functor, Ord, Show)

makeLenses ''CoordsF

instance Foldable CoordsF where
  foldMap f = getConst . traverse (Const . f)

instance Traversable CoordsF where
  traverse f coords@CoordsF { .. } = (\_z -> coords { _z = _z }) <$> f _z
  sequence coords@CoordsF { .. } = (\_z -> coords { _z = _z }) <$> _z

type Step = CoordsF Direction

type Bounds = (Coords, Coords)

type Grid = Array Coords TileHistory

data GridM where
  GridM :: (PrimMonad m, MArray a TileHistory m)
    => a Coords TileHistory
    -> GridM

toGrid :: [[TileHistory]] -> Grid
toGrid = (array =<< bounds')
  <<< join . zipWith (fmap . first . (,)) (enumFrom 0) . fmap (zip (enumFrom 0))
  where
    bounds' a = fromMaybe ((0, 1), (0, 0)) $ do
      head <- fst . fst <$> uncons a
      last <- fst . snd <$> unsnoc a
      return (head, last)

class (PrimBase m, MArray a TileHistory m) => BeamMonad a m
instance (PrimBase m, MArray a TileHistory m) => BeamMonad a m

type Input = Grid

type OutputA = Word

type OutputB = Word

------------ PARSER ------------
tileHistory :: Parser TileHistory
tileHistory = do
  _tile <- toTile <$> anyChar
  guard (Error /= _tile) $> TileHistory { .. }
  where
    _energized = mempty

row :: Parser [TileHistory]
row = many tileHistory <* endOfLine

grid :: Parser Grid
grid = toGrid <$> many row

inputParser :: Parser Input
inputParser = grid

------------ PART A ------------
turn :: Tile -> Direction -> [Direction]
turn Space d = [d]
turn RMirror North = [East]
turn RMirror East = [North]
turn RMirror South = [West]
turn RMirror West = [South]
turn LMirror North = [West]
turn LMirror East = [South]
turn LMirror South = [East]
turn LMirror West = [North]
turn HSplitter North = [East, West]
turn HSplitter South = [East, West]
turn HSplitter East = [East]
turn HSplitter West = [West]
turn VSplitter North = [North]
turn VSplitter South = [South]
turn VSplitter East = [North, South]
turn VSplitter West = [North, South]

nextStep :: Bounds -> Step -> [Step]
nextStep ((northBound, _), _) coords@CoordsF { _z = North, .. }
  | _y <= northBound = []
  | otherwise = [coords & y %~ pred]
nextStep ((_, westBound), _) coords@CoordsF { _z = West, .. }
  | _x <= westBound = []
  | otherwise = [coords & x %~ pred]
nextStep (_, (southBound, _)) coords@CoordsF { _z = South, .. }
  | _y >= southBound = []
  | otherwise = [coords & y %~ succ]
nextStep (_, (_, eastBound)) coords@CoordsF { _z = East, .. }
  | _x >= eastBound = []
  | otherwise = [coords & x %~ succ]

turnAndStep :: Bounds -> Tile -> Step -> [Step]
turnAndStep b t coords =
  turn t (coords ^. z) >>= nextStep b . flip (set z) coords

tick :: (BeamMonad a m) => a Coords TileHistory -> Step -> m [Step]
tick a coords@CoordsF { .. } = do
  let c = (_y, _x)
  b <- getBounds a
  t@TileHistory { .. } <- readArray a c
  if t ^. energized . visited _z
    then return []
    else do
      modifyArray a c (energized . visited _z .~ True)
      return $ turnAndStep b _tile coords

ticks :: (BeamMonad a m) => a Coords TileHistory -> [Step] -> m [Step]
ticks a = fmap join . traverse (tick a)

shine :: (BeamMonad a m)
  => a Coords TileHistory
  -> [Step]
  -> m (a Coords TileHistory)
shine a [] = return a
shine a steps = ticks a steps >>= shine a

start :: Step
start = CoordsF { _y = 0, _x = 0, _z = East }

illuminate :: Step -> Grid -> Grid
illuminate start a' = runSTArray $ do
  a <- thaw a'
  shine a [start]

countEnergized :: Grid -> Word
countEnergized =
  fromIntegral . lengthOf (traverse . energized . filtered (dark /=))

partA :: Input -> OutputA
partA = countEnergized . illuminate start

------------ PART B ------------
northStart :: Bounds -> [Step]
northStart ((northBound, westBound), (_, eastBound)) =
  flip (set x) coord <$> enumFromTo westBound eastBound
    where
      coord = CoordsF { _y = northBound, _x = westBound, _z = South }

eastStart :: Bounds -> [Step]
eastStart ((northBound, _), (southBound, eastBound)) =
  flip (set y) coord <$> enumFromTo northBound southBound
    where
      coord = CoordsF { _y = northBound, _x = eastBound, _z = West }

southStart :: Bounds -> [Step]
southStart ((_, westBound), (southBound, eastBound)) =
  flip (set x) coord <$> enumFromTo westBound eastBound
    where
      coord = CoordsF { _y = southBound, _x = westBound, _z = North }

westStart :: Bounds -> [Step]
westStart ((northBound, westBound), (southBound, _)) =
  flip (set y) coord <$> enumFromTo northBound southBound
    where
      coord = CoordsF { _y = northBound, _x = westBound, _z = East }

boundaries :: Bounds -> [Step]
boundaries b = [northStart, eastStart, southStart, westStart] >>= ($ b)

partB :: Input -> OutputB
partB input = fromMaybe 0
  . maximumOf (traverse . to (countEnergized . flip illuminate input))
  . boundaries $ bounds input

runDay :: R.Day
runDay = R.runDay inputParser partA partB
