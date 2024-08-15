{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use infix" #-}

module Days.Day12 (runDay) where

import           Control.Applicative
import           Control.Lens
import           Data.Attoparsec.Text hiding (take)
import           Data.Coerce
import           Data.Fix
import           Data.Functor
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Void
import           GHC.Read
import qualified Text.ParserCombinators.ReadPrec as RP
import qualified Program.RunDay as R (runDay, Day)
import qualified Util.Util as U

------------ TYPES ------------
data SpringCondition where
  Damaged :: SpringCondition     -- #
  Operational :: SpringCondition -- .
  Unknown :: SpringCondition     -- ?
  Error :: SpringCondition       -- ¿
  deriving (Enum, Bounded, Eq, Ord)

makeLenses ''SpringCondition

instance Show SpringCondition where
  show = (:[]) . fromSpringCondition

  showList ls s = fmap fromSpringCondition ls ++ s

instance Read SpringCondition where
  readPrec = toSpringCondition <$> RP.get

  readListPrec = many readPrec

toSpringCondition :: Char -> SpringCondition
toSpringCondition '#' = Damaged
toSpringCondition '.' = Operational
toSpringCondition '?' = Unknown
toSpringCondition _ = Error

fromSpringCondition :: SpringCondition -> Char
fromSpringCondition Damaged = '#'
fromSpringCondition Operational = '.'
fromSpringCondition Unknown = '?'
fromSpringCondition Error = '¿'

data ConditionRecord where
  ConditionRecord :: { _row :: [SpringCondition], _groups :: [Word] }
    -> ConditionRecord
  deriving (Eq, Ord, Show)

makeLenses ''ConditionRecord

data SpringTrieF a b where
  Tip :: SpringTrieF a b
  Leaf :: a -> SpringTrieF a b
  NextDamaged :: Word -> b -> SpringTrieF a b
  NextOperational :: Word -> b -> SpringTrieF a b
  NextUnknown :: Word -> b -> b -> SpringTrieF a b
  deriving (Eq, Ord, Show, Functor)

type SpringTrie a = Fix (SpringTrieF a)

type Algebra f a = f a -> a

type Coalgebra f a = a -> f a

type SpringAlgebra a b = Algebra (SpringTrieF a) b

type SpringCoalgebra a b = Coalgebra (SpringTrieF a) b

type Input = [ConditionRecord]

type OutputA = Word

type OutputB = Void

------------ PARSER ------------
springCondition :: Parser SpringCondition
springCondition =
  char '#' $> Damaged <|> char '.' $> Operational <|> char '?' $> Unknown

conditionRecord :: Parser ConditionRecord
conditionRecord = do
  _row <- many springCondition
  space
  _groups <- sepBy decimal $ char ','
  endOfLine
  return ConditionRecord { .. }

inputParser :: Parser Input
inputParser = many conditionRecord

------------ PART A ------------
springTrieAlg :: SpringAlgebra () Word
springTrieAlg = \case
  Tip -> 0
  Leaf _ -> 1
  NextDamaged _ x -> x
  NextOperational _ y -> y
  NextUnknown _ x y -> x + y

springTrieCoalg :: SpringCoalgebra () ConditionRecord
springTrieCoalg ConditionRecord { .. } = case _row of
  []   -> case _groups of
    [] -> Leaf ()
    _  -> Tip
  s:ss -> case _groups of
    [] -> if elem Damaged _row
          then Tip
          else done
    l:ls -> case s of
      Damaged     -> if canHaveDamagedSequence l _row
                     then NextDamaged l damaged
                     else Tip
      Operational -> NextOperational 1 operational
      Unknown     -> if canHaveDamagedSequence l _row
                     then NextUnknown l damaged operational
                     else NextOperational 1 operational
      Error       -> Tip
      where
        operational = ConditionRecord { _row = ss, .. }

        damaged = ConditionRecord { _row = replaceHead rest, _groups = ls }

        rest = drop (fromIntegral l) _row

        replaceHead [] = []
        replaceHead (_:xs) = Operational:xs
  where
    done = NextOperational
      (fromIntegral $ length _row)
      ConditionRecord { _row = [], _groups = [] }

canHaveDamagedSequence :: Word -> [SpringCondition] -> Bool
canHaveDamagedSequence l springs = i <= length springs
  && maybe True (> 0) (elemIndex Damaged $ drop i springs)
  && maybe True (>= i) (elemIndex Operational springs)
  where
    i = fromIntegral l

countInputs :: ConditionRecord -> Word
countInputs = refold springTrieAlg springTrieCoalg

possibleInputs :: ConditionRecord -> [[SpringCondition]]
possibleInputs = catMaybes . refold alg springTrieCoalg
  where
    alg
      :: SpringTrieF () [Maybe [SpringCondition]] -> [Maybe [SpringCondition]]
    alg = \case
      Tip -> [Nothing]
      Leaf _ -> [Just []]
      NextDamaged x ls
        -> (fmap . fmap) (replicate (fromIntegral x) Damaged ++) ls
      NextOperational y rs
        -> (fmap . fmap) (replicate (fromIntegral y) Operational ++) rs
      NextUnknown x ls rs
        -> (fmap . fmap) (replicate (fromIntegral x) Damaged ++) ls
        ++ (fmap . fmap) (Operational:) rs

partA :: Input -> OutputA
partA = getSum . foldMap (coerce . countInputs)

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Unimplemented"

runDay :: R.Day
runDay = R.runDay inputParser partA partB
