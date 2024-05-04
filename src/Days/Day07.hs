{-# LANGUAGE TemplateHaskell #-}
module Days.Day07 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative
import Control.Lens
import Control.Monad
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

------------ TYPES ------------
data Card where
  Two :: Card
  Three :: Card
  Four :: Card
  Five :: Card
  Six :: Card
  Seven :: Card
  Eight :: Card
  Nine :: Card
  Ten :: Card
  Jack :: Card
  Queen :: Card
  King :: Card
  Ace :: Card
  deriving (Eq, Enum, Ord)
makePrisms ''Card

instance Show Card where
  show Two = "2"
  show Three = "3"
  show Four = "4"
  show Five = "5"
  show Six = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine = "9"
  show Ten = "T"
  show Jack = "J"
  show Queen = "Q"
  show King = "K"
  show Ace = "A"

data HandType where
  HighCard :: HandType
  OnePair :: HandType
  TwoPair :: HandType
  ThreeOfAKind :: HandType
  FullHouse :: HandType
  FourOfAKind :: HandType
  FiveOfAKind :: HandType
  deriving (Eq, Enum, Ord, Show)
makePrisms ''HandType

instance Semigroup HandType where
  OnePair <> OnePair = TwoPair
  OnePair <> ThreeOfAKind = FullHouse
  ThreeOfAKind <> OnePair = FullHouse
  a <> b
    | a < b = b
    | a >= b = a

instance Monoid HandType where
  mempty = HighCard

data Hand where
  Hand :: {
    _handtype :: HandType,
    _cards :: [Card]
  } -> Hand
  deriving (Eq, Ord, Show)
makeLenses ''Hand

data Bid where
  Bid :: {
    _hand :: Hand,
    _bid :: Int
  } -> Bid
  deriving (Eq, Ord, Show)
makeLenses ''Bid

type Input = [Bid]

type OutputA = Int

type OutputB = Void

------------ PARSER ------------
parseCard :: Parser Card
parseCard = parseCard' <$> anyChar
  where
    parseCard' '2' = Two
    parseCard' '3' = Three
    parseCard' '4' = Four
    parseCard' '5' = Five
    parseCard' '6' = Six
    parseCard' '7' = Seven
    parseCard' '8' = Eight
    parseCard' '9' = Nine
    parseCard' 'T' = Ten
    parseCard' 'J' = Jack
    parseCard' 'Q' = Queen
    parseCard' 'K' = King
    parseCard' 'A' = Ace

getHandType :: [Card] -> HandType
getHandType = mconcat . map (f . length) . group . sort
  where
    f :: Int -> HandType
    f 0 = HighCard
    f 1 = HighCard
    f 2 = OnePair
    f 3 = ThreeOfAKind
    f 4 = FourOfAKind
    f 5 = FiveOfAKind

parseHand :: Parser Hand
parseHand = (getHandType >>= Hand) <$> replicateM 5 parseCard

parseBid :: Parser Bid
parseBid = do
  _hand <- parseHand
  skipSpace
  _bid <- decimal
  endOfLine
  return Bid { _hand, _bid }

inputParser :: Parser Input
inputParser = many parseBid

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PART A ------------
partA :: Input -> OutputA
-- partA = error "Not implemented yet!"
partA = sum . zipWith (*) (enumFrom 1) . toListOf (folded . bid) . sort

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
