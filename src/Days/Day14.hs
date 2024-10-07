{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Days.Day14 (runDay) where

import Control.Arrow
import Control.Lens hiding (cons,uncons)
import Control.Monad
import Control.Monad.Combinators

import Data.Array.IArray
import Data.Attoparsec.Text (Parser,anyChar,char,endOfLine)
import Data.Bifunctor
import Data.Coerce
import Data.Function
import Data.Functor
import Data.Functor.Contravariant
import Data.List
import Data.List.Split (splitWhen)
import Data.Monoid
import Data.Void

import qualified Program.RunDay as R (Day,runDay)

import qualified Text.ParserCombinators.ReadP as R
import qualified Text.ParserCombinators.ReadPrec as RP
import Text.Read

import qualified Util.Util as U

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

newtype Grid = Grid
  { unGrid :: Array (Word, Word) Rock
  }

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
  show = intercalate "\n" . fmap (show . fmap snd) . groupBy
    (on (==) $ fst . fst) . assocs . unGrid

  showList ls s = intercalate "\n\n" (fmap show ls) ++ s

toGrid :: [[Rock]] -> Grid
toGrid g = Grid $ array bounds $ zip (range bounds) $ join g
  where
    bounds = ((0, 0), on (,) (fromIntegral . pred . length) <*> transpose $ g)

type Input = Grid

type OutputA = Word

type OutputB = Void

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
takeDrop :: Int -> [a] -> ([a], [a])
takeDrop n = take n &&& drop n

getCubesByLongitude :: Grid -> [[(Word, Word)]]
getCubesByLongitude = groupBy (on (==) snd) . sortBy comparison . map fst
  . filter ((Cube ==) . snd) . assocs . unGrid
  where
    comparison = getComparison $ (snd >$< defaultComparison)
      <> (fst >$< defaultComparison)

load :: Grid -> Word -> Word
load = (-) . (1 +) . uncurry subtract <<< each %~ fst <<< bounds . unGrid

columnsSplitByCube :: Grid -> [[[((Word, Word), Rock)]]]
columnsSplitByCube = fmap (splitWhen ((Cube ==) . snd))
  . groupBy (on (==) $ snd . fst) . sortBy comparison . assocs . unGrid
  where
    comparison = getComparison
      $ (snd . fst >$< defaultComparison)
      <> (fst . fst >$< defaultComparison)

loadOfTiltedSegment :: Grid -> [((Word, Word), Rock)] -> Word
loadOfTiltedSegment g = coerce . foldMap (Sum . load g . fst . fst)
  <<< take =<< lengthOf (traverse . _2 . filtered (Round ==))

loadOfGrid :: Grid -> Word
loadOfGrid g = coerce . foldMap (foldMap (Sum . loadOfTiltedSegment g))
  . columnsSplitByCube $ g

partA :: Input -> OutputA
partA = loadOfGrid

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"

runDay :: R.Day
runDay = R.runDay inputParser partA partB
