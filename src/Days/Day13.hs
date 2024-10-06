{-# LANGUAGE GADTs #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Days.Day13 (runDay) where

import Control.Arrow
import Control.Lens hiding (cons,uncons)
import Control.Monad
import Control.Monad.Combinators
import Control.Monad.ST

import Data.Array.IArray
import Data.Array.MArray
import Data.Array.ST
import Data.Attoparsec.Text (Parser,char,endOfLine)
import Data.Coerce
import Data.Function
import Data.Functor
import Data.Ix
import Data.List
import Data.Maybe
import Data.Monoid

import qualified Program.RunDay as R (Day,runDay)

import qualified Text.ParserCombinators.ReadP as R
import qualified Text.ParserCombinators.ReadPrec as RP
import Text.Read

import qualified Util.Util as U

------------ TYPES ------------
data Ground where
  Ash :: Ground    -- .
  Rock :: Ground   -- #
  Smudge :: Ground -- $
  Error :: Ground  -- ¿
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
toGround '$' = Rock
toGround _ = Error

fromGround :: Ground -> Char
fromGround Ash = '.'
fromGround Rock = '#'
fromGround Smudge = '$'
fromGround Error = '¿'

newtype Grid = Grid
  { unGrid :: Array (Word, Word) Ground
  }
  deriving (Eq,Ord)

instance Read Grid where
  readPrec = toGrid <$> some row
    where
      cell = toGround <$> RP.get

      row = some cell <* endOfLine

      endOfLine = lift $ R.char '\n' $> () <|> R.string "\r\n" $> ()

  readListPrec = sepBy readPrec endOfLine
    where
      endOfLine = lift $ R.char '\n' $> () <|> R.string "\r\n" $> ()

instance Show Grid where
  show = intercalate "\n" . fmap (show . fmap snd) . groupBy
    (on (==) $ fst . fst) . assocs . unGrid

  showList ls s = intercalate "\n\n" (fmap show ls) ++ s

type Input = [Grid]

type OutputA = Word

type OutputB = Word

------------ PARSER ------------
toGrid :: [[Ground]] -> Grid
toGrid g = Grid $ array bounds $ zip (range bounds) $ join g
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
northReflectionIndices (Grid g) y' = do
  y <- reverse . take (fromIntegral y') . range <<< each %~ fst $ bounds g
  x <- range <<< each %~ snd $ bounds g
  return (y, x)

southReflectionIndices :: Grid -> Word -> [(Word, Word)]
southReflectionIndices (Grid g) y' = do
  y <- drop (fromIntegral y') . range <<< each %~ fst $ bounds g
  x <- range <<< each %~ snd $ bounds g
  return (y, x)

westReflectionIndices :: Grid -> Word -> [(Word, Word)]
westReflectionIndices (Grid g) x' = do
  x <- reverse . take (fromIntegral x') . range <<< each %~ snd $ bounds g
  y <- range <<< each %~ fst $ bounds g
  return (y, x)

eastReflectionIndices :: Grid -> Word -> [(Word, Word)]
eastReflectionIndices (Grid g) x' = do
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
isSymmetricOnLongitude g = all (uncurry (on (==) (unGrid g !)))
  . reflectLongitudeIndices g

isSymmetricOnLatitude :: Grid -> Word -> Bool
isSymmetricOnLatitude g = all (uncurry (on (==) (unGrid g !)))
  . reflectLatitudeIndices g

getLongitudes :: Grid -> [Word]
getLongitudes = range <<< _1 %~ (1 +) <<< each %~ snd <<< bounds . unGrid

getLatitudes :: Grid -> [Word]
getLatitudes = range <<< _1 %~ (1 +) <<< each %~ fst <<< bounds . unGrid

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
assymetriesOnLongitude :: Grid -> Word -> [((Word, Word), (Word, Word))]
assymetriesOnLongitude g = filter (uncurry (on (/=) (unGrid g !)))
  . reflectLongitudeIndices g

assymetriesOnLatitude :: Grid -> Word -> [((Word, Word), (Word, Word))]
assymetriesOnLatitude g = filter (uncurry (on (/=) (unGrid g !)))
  . reflectLatitudeIndices g

getLongitudeSmudges :: Grid -> [((Word, Word), (Word, Word))]
getLongitudeSmudges g = do
  longAssymetries <- assymetriesOnLongitude g <$> getLongitudes g
  guard (length longAssymetries == 1) *> longAssymetries

getLatitudeSmudges :: Grid -> [((Word, Word), (Word, Word))]
getLatitudeSmudges g = do
  latAssymetries <- assymetriesOnLatitude g <$> getLatitudes g
  guard (length latAssymetries == 1) *> latAssymetries

getSmudges :: Grid -> [((Word, Word), (Word, Word))]
getSmudges g = nub $ getLongitudeSmudges g <|> getLatitudeSmudges g

flipGround :: Ground -> Ground
flipGround Ash = Rock
flipGround Rock = Ash
flipGround x = x

cleanSmudges :: Grid -> Grid
cleanSmudges g' = Grid $ runSTArray $ do
  let smudges = getSmudges g'
  g <- thaw $ unGrid g'
  mapM_ (flip (modifyArray g) flipGround . fst) smudges
  return g

getLongitudesWithSmudge :: Grid -> [Word]
getLongitudesWithSmudge g = filter
  (\l -> length (assymetriesOnLongitude g l) == 1) . getLongitudes $ g

getLatitudesWithSmudge :: Grid -> [Word]
getLatitudesWithSmudge g = filter
  (\l -> length (assymetriesOnLatitude g l) == 1) . getLatitudes $ g

symmetryScoreWithSmudges :: Grid -> Word
symmetryScoreWithSmudges g = sum (getLongitudesWithSmudge g)
  + 100 * sum (getLatitudesWithSmudge g)

partB :: Input -> OutputB
partB = getSum . foldMap (coerce . symmetryScoreWithSmudges)

runDay :: R.Day
runDay = R.runDay inputParser partA partB
