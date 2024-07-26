{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Days.Day11 (runDay) where

import Control.Arrow
import Control.Lens hiding (index, uncons)
import Control.Monad
import Data.Array.IArray hiding (index)
import Data.Attoparsec.Text
import Data.Distributive
import Data.Function
import Data.Functor
import Data.Functor.Adjunction
import Data.Functor.Rep
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Void
import GHC.Num.Natural (naturalFromWord, naturalToWord)
import GHC.TypeNats

import qualified Data.Text as T
import qualified Program.RunDay as R (runDay, Day)
import qualified Util.Util as U

------------ TYPES ------------
type Coords = (Word, Word)

knownNatToWord :: forall n. (KnownNat n) => Word
knownNatToWord = naturalToWord $ fromSNat @n natSing

knownNatToWord' :: forall n. (KnownNat n) => Word
knownNatToWord' = naturalToWord $ fromSNat @n natSing - 1

class D2Bound len wid where
  d2Length :: Word
  d2Width :: Word
  northBound :: Word
  eastBound :: Word
  southBound :: Word
  westBound :: Word
  d2Bounds :: (Coords, Coords)

instance (KnownNat len, KnownNat wid) => D2Bound len wid where
  d2Length = knownNatToWord @len
  d2Width = knownNatToWord @wid
  northBound = 0
  eastBound = knownNatToWord' @wid
  southBound = knownNatToWord' @len
  westBound = 0
  d2Bounds = ((0, 0), (knownNatToWord' @len, knownNatToWord' @wid))

data CoordF (len :: Nat) (wid :: Nat) a where
  CoordF :: {
    _y :: Word,
    _x :: Word,
    _z :: a
  } -> CoordF len wid a
  deriving (Eq, Functor, Ord, Show)
makeLenses ''CoordF

type Coord len wid = CoordF len wid ()

newtype ChartF (len :: Nat) (wid :: Nat) a =
  ChartF { unChartF :: Array Coords a }
  deriving (Functor)
makeLenses ''ChartF

instance (D2Bound len wid) => Show (ChartF len wid a) where
  show _ = "chart"

instance (D2Bound len wid) =>
  Adjunction (CoordF len wid) (ChartF len wid) where
  unit _z = ChartF $ genArray (d2Bounds @len @wid) $ \(_y, _x) -> CoordF {..}
  counit CoordF {..} = unChartF _z ! (_y, _x)
  leftAdjunct f _z = ChartF $ genArray (d2Bounds @len @wid) $ \(_y, _x) -> f CoordF {..}
  rightAdjunct f CoordF {..} = unChartF (f _z) ! (_y, _x)

instance (D2Bound len wid) =>
  Representable (ChartF len wid) where
  type Rep (ChartF len wid) = Coord len wid
  tabulate = tabulateAdjunction
  index = indexAdjunction

instance (D2Bound len wid) =>
  Distributive (ChartF len wid) where
  distribute = distributeRep
  collect = collectRep

data Aster where
  Error :: Aster
  Voidspace :: Aster
  Galaxy :: Aster
  deriving (Bounded, Enum, Eq, Ord, Show)
makePrisms ''Aster

data ExpansionGrid where
  ExpansionGrid :: {
    _latitudes :: [Word],
    _longitudes :: [Word]
  } -> ExpansionGrid
  deriving (Eq, Ord, Show)
makeLenses ''ExpansionGrid

data ExpanseChart where
  ExpanseChart :: forall len wid. (D2Bound len wid) => {
    chart :: Chart len wid,
    astra :: [CoordF len wid Aster],
    expanse :: ExpansionGrid
  } -> ExpanseChart

instance Show ExpanseChart where
  show _ = "expanse chart"

type Chart len wid = ChartF len wid Aster

type Input = ExpanseChart

type OutputA = Word

type OutputB = Word

------------ PARSER ------------
toAster :: Char -> Aster
toAster '.' = Voidspace
toAster '#' = Galaxy
toAster _ = Error

fromAster :: Aster -> Char
fromAster Voidspace = '.'
fromAster Galaxy = '#'
fromAster Error = '?'

findWidth :: MonadFail m => T.Text -> m Word
findWidth = maybe (fail "Could not find newline.") (return . fromIntegral)
  . T.findIndex (== '\n')

findAtCoord :: forall len wid a. (D2Bound len wid) =>
  T.Text -> CoordF len wid a -> Aster
findAtCoord text CoordF {..} = toAster $ T.index text $ fromIntegral
  $ succ (d2Width @len @wid) * _y + _x

isExpansionLatitude :: forall len wid. (D2Bound len wid) =>
  Chart len wid -> Word -> Bool
isExpansionLatitude chart _y =
  all ((Voidspace ==) . index chart . atLongitude) . enumFromTo 0
    $ eastBound @len @wid
  where
    atLongitude _x = CoordF {..}
    _z = ()

isExpansionLongitude :: forall len wid. (D2Bound len wid) =>
  Chart len wid -> Word -> Bool
isExpansionLongitude chart _x =
  all ((Voidspace ==) . index chart . atLatitude) . enumFromTo 0
    $ southBound @len @wid
  where
    atLatitude _y = CoordF {..}
    _z = ()

findExpansionLatitudes :: forall len wid. (D2Bound len wid) =>
  Chart len wid -> [Word]
findExpansionLatitudes chart =
  filter (isExpansionLatitude chart) . enumFromTo 0 $ southBound @len @wid

findExpansionLongitudes :: forall len wid. (D2Bound len wid) =>
  Chart len wid -> [Word]
findExpansionLongitudes chart =
  filter (isExpansionLongitude chart) . enumFromTo 0 $ eastBound @len @wid

findExpansionGrid :: forall len wid. (D2Bound len wid) =>
  Chart len wid -> ExpansionGrid
findExpansionGrid =
  ExpansionGrid <$> findExpansionLatitudes <*> findExpansionLongitudes

findAstra :: forall len wid. (D2Bound len wid) =>
  Chart len wid -> [CoordF len wid Aster]
findAstra = fmap toCoordF . filter ((Galaxy ==) . snd) . assocs . unChartF
  where
    toCoordF ((_y, _x), _z) = CoordF {..}

parseChart :: Parser ExpanseChart
parseChart = do
  text <- takeText
  w <- findWidth text
  let
    l = div (fromIntegral $ T.length text) (w + 1)
    len = naturalFromWord l
    wid = naturalFromWord w
  withSomeSNat len $ \(SNat @len) ->
    withSomeSNat wid $ \(SNat @wid) ->
      let
        chart = tabulate $ findAtCoord text
        astra = findAstra chart
        expanse = findExpansionGrid chart
      in
        return $ ExpanseChart @len @wid chart astra expanse

------------ PART A ------------
allPairs :: [a] -> [(a, a)]
allPairs = uncurry (fmap . (,)) <=< mapMaybe uncons . tails

rearrangeCoords :: (Coords, Coords) -> (Coords, Coords)
rearrangeCoords ((a, b), (c, d)) = ((a', b'), (c', d'))
  where
    (a', c') = if a <= c
      then (a, c)
      else (c, a)
    (b', d') = if b <= d
      then (b, d)
      else (d, b)

astralPairs :: (D2Bound len wid) => [CoordF len wid Aster] -> [(Coords, Coords)]
astralPairs = fmap rearrangeCoords . allPairs . fmap toCoords
  where
    toCoords CoordF {..} = (_y, _x)

astralDistance :: ExpansionGrid -> Word -> Coords -> Coords -> Word
astralDistance ExpansionGrid {..} expansion (a, b) (c, d) =
  c - a +
    ((pred expansion *) . fromIntegral . length . filter (inRange (a, c))
    $ _latitudes)
  + d - b +
    ((pred expansion *) . fromIntegral . length . filter (inRange (b, d))
    $ _longitudes)

getGalaxyDistances :: Word -> ExpanseChart -> Word
getGalaxyDistances expansion ExpanseChart {..} =
  getSum . foldMap (Sum . uncurry (astralDistance expanse expansion))
    . astralPairs $ astra

partA :: Input -> OutputA
partA = getGalaxyDistances 2

------------ PART B ------------
partB :: Input -> OutputB
partB = getGalaxyDistances 1_000_000

runDay :: R.Day
runDay = R.runDay parseChart partA partB
