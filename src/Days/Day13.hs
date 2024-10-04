{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Days.Day13 (runDay) where

import Control.Applicative
import Control.Arrow
import Control.Lens hiding (cons,snoc,uncons,unsnoc)
import Control.Monad

import Data.Array.IArray
import Data.Attoparsec.Text hiding (take)
import Data.Coerce
import Data.Function
import Data.Functor
import Data.Ix
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Void

import qualified Program.RunDay as R (Day,runDay)

import qualified Text.ParserCombinators.ReadPrec as RP
import Text.Read

import qualified Util.Util as U

------------ TYPES ------------
data Ground where
  Ash :: Ground   -- .
  Rock :: Ground  -- #
  Error :: Ground -- ¿
  deriving (Bounded,Enum,Eq,Ord)

makePrisms ''Ground

instance Read Ground where
  readPrec = toGround <$> RP.get

  readListPrec = many readPrec

instance Show Ground where
  show = (: []) . fromGround

  showList ls s = fmap fromGround ls ++ s

toGround :: Char -> Ground
toGround '.' = Ash
toGround '#' = Rock
toGround _ = Error

fromGround :: Ground -> Char
fromGround Ash = '.'
fromGround Rock = '#'
fromGround Error = '¿'

type Grid = Array (Word, Word) Ground

type Input = [Grid]

type OutputA = Word

type OutputB = Void

------------ PARSER ------------
toGrid :: [[Ground]] -> Grid
toGrid g = array bounds $ zip (range bounds) $ join g
  where
    bounds = ((0, 0), on (,) (fromIntegral . pred . length) <*> transpose $ g)

ash :: Parser Ground
ash = char '.' $> Ash

rock :: Parser Ground
rock = char '#' $> Rock

ground :: Parser Ground
ground = ash <|> rock

row :: Parser [Ground]
row = some ground <* endOfLine

grid :: Parser Grid
grid = toGrid <$> many row

grids :: Parser [Grid]
grids = sepBy grid endOfLine

inputParser :: Parser Input
inputParser = grids

------------ PART A ------------
northReflectionIndices :: Grid -> Word -> [(Word, Word)]
northReflectionIndices g y' = do
  y <- reverse . take (fromIntegral y') . range <<< each %~ fst $ bounds g
  x <- range <<< each %~ snd $ bounds g
  return (y, x)

southReflectionIndices :: Grid -> Word -> [(Word, Word)]
southReflectionIndices g y' = do
  y <- drop (fromIntegral y') . range <<< each %~ fst $ bounds g
  x <- range <<< each %~ snd $ bounds g
  return (y, x)

westReflectionIndices :: Grid -> Word -> [(Word, Word)]
westReflectionIndices g x' = do
  x <- reverse . take (fromIntegral x') . range <<< each %~ snd $ bounds g
  y <- range <<< each %~ fst $ bounds g
  return (y, x)

eastReflectionIndices :: Grid -> Word -> [(Word, Word)]
eastReflectionIndices g x' = do
  x <- drop (fromIntegral x') . range <<< each %~ snd $ bounds g
  y <- range <<< each %~ fst $ bounds g
  return (y, x)

reflectLongitudeIndices :: Grid -> Word -> [((Word, Word), (Word, Word))]
reflectLongitudeIndices g x =
  zip (westReflectionIndices g x) (eastReflectionIndices g x)

reflectLatitudeIndices :: Grid -> Word -> [((Word, Word), (Word, Word))]
reflectLatitudeIndices g x =
  zip (northReflectionIndices g x) (southReflectionIndices g x)

isSymmetricOnLongitude :: Grid -> Word -> Bool
isSymmetricOnLongitude g = all (uncurry (on (==) (g !)))
  . reflectLongitudeIndices g

isSymmetricOnLatitude :: Grid -> Word -> Bool
isSymmetricOnLatitude g = all (uncurry (on (==) (g !)))
  . reflectLatitudeIndices g

getLongitudes :: Grid -> [Word]
getLongitudes = range <<< _1 %~ succ <<< each %~ snd <<< bounds

getLatitudes :: Grid -> [Word]
getLatitudes = range <<< _1 %~ succ <<< each %~ fst <<< bounds

longitudeSymmetryScore :: Grid -> Word
longitudeSymmetryScore g = sum . filter (isSymmetricOnLongitude g)
  $ getLongitudes g

latitudeSymmetryScore :: Grid -> Word
latitudeSymmetryScore g = sum . map (100 *) . filter (isSymmetricOnLatitude g)
  $ getLatitudes g

symmetryScore :: Grid -> Word
symmetryScore g = longitudeSymmetryScore g + latitudeSymmetryScore g

partA :: Input -> OutputA
partA = getSum . foldMap (coerce . symmetryScore)

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"

runDay :: R.Day
runDay = R.runDay inputParser partA partB
