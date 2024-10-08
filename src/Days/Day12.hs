{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use infix" #-}

module Days.Day12 (runDay) where

import Control.Applicative
import Control.Arrow
import Control.Lens hiding (cons,snoc,uncons,unsnoc)
import Control.Monad
import Control.Monad.Codensity
import Control.Monad.Memo

import Data.Attoparsec.Text hiding (Fail,take)
import Data.Coerce
import Data.Fix
import Data.Function
import Data.Functor
import Data.List
import Data.List.NonEmpty (NonEmpty((:|)),nonEmpty,toList)
import Data.Maybe
import Data.Monoid

import qualified Program.RunDay as R (Day,runDay)

import qualified Text.ParserCombinators.ReadPrec as RP
import Text.Read

import qualified Util.Util as U

------------ TYPES ------------
data SpringCondition where
  Damaged :: SpringCondition     -- #
  Operational :: SpringCondition -- .
  Unknown :: SpringCondition     -- ?
  Error :: SpringCondition       -- ¿
  deriving (Enum,Bounded,Eq,Ord)

makePrisms ''SpringCondition

instance Read SpringCondition where
  readPrec = toSpringCondition <$> RP.get

  readListPrec = many readPrec

instance Show SpringCondition where
  show = (: []) . fromSpringCondition

  showList ls s = fmap fromSpringCondition ls ++ s

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
  ConditionRecord :: {
                     _row :: [SpringCondition],
                     _groups :: [Word]
                     } -> ConditionRecord
  deriving (Eq,Ord,Show)

makeLenses ''ConditionRecord

data SpringParserF a where
  Fail :: SpringParserF a
  GetCondition :: (SpringCondition -> SpringParserF a) -> SpringParserF a
  GetGroup :: (Word -> SpringParserF a) -> SpringParserF a
  Peek :: (ConditionRecord -> SpringParserF a) -> SpringParserF a
  Fork :: SpringParserF a -> SpringParserF a -> SpringParserF a
  Result :: a -> SpringParserF a -> SpringParserF a
  Final :: NonEmpty (a, ConditionRecord) -> SpringParserF a
  deriving (Functor)

instance Applicative SpringParserF where
  pure x = Result x Fail

  (<*>) = ap

instance Monad SpringParserF => Monad SpringParserF where
  Fail >>= _ = Fail
  GetCondition f >>= k = GetCondition (f >=> k)
  GetGroup f >>= k = GetGroup (f >=> k)
  Peek f >>= k = Peek (f >=> k)
  Fork p q >>= k = Fork (p >>= k) (q >>= k)
  Result x p >>= k = k x <|> (p >>= k)
  Final rs >>= k = final $ toList rs >>= uncurry (run . k)

instance Alternative SpringParserF where
  empty = Fail

  GetCondition f <|> GetCondition g = GetCondition $ uncurry (<|>) <<< f &&& g
  GetGroup f <|> GetGroup g = GetGroup $ uncurry (<|>) <<< f &&& g
  Result x p <|> q = Result x (p <|> q)
  p <|> Result x q = Result x (p <|> q)
  Fail <|> p = p
  p <|> Fail = p
  Final r <|> Final s = Final (r <> s)
  Final r <|> p = Peek $ Final . maybe r (r <>) . nonEmpty . run p
  p <|> Final r = Peek $ Final . maybe r (<> r) . nonEmpty . run p
  Peek f <|> Peek g = Peek $ uncurry (<|>) <<< f &&& g
  Peek f <|> p = Peek $ (<|> p) . f
  p <|> Peek f = Peek $ (p <|>) . f
  c@GetCondition{} <|> g@GetGroup{} = Fork g c
  g@GetGroup{} <|> c@GetCondition{} = Fork g c
  Fork p q <|> Fork r s = Fork (p <|> r) (q <|> s)
  Fork p q <|> r = Fork (p <|> r) (q <|> r)
  r <|> Fork p q = Fork (r <|> p) (r <|> q)

instance MonadPlus SpringParserF

instance MonadFail SpringParserF where
  fail = const Fail

instance (Show a) => Show (SpringParserF a) where
  show Fail = "Fail"
  show (GetCondition _) = "GetCondition"
  show (GetGroup _) = "GetGroup"
  show (Peek _) = "Peek"
  show (Fork p q) = "Fork: " ++ show p ++ " " ++ show q
  show (Result x p) = "Result: " ++ show x ++ " " ++ show p
  show (Final r) = "Final: " ++ show r

data SpringTrieF a where
  Tip :: SpringTrieF a
  Leaf :: SpringTrieF a
  Pure :: a -> SpringTrieF a
  Node :: a -> a -> SpringTrieF a
  deriving (Functor)

instance Foldable SpringTrieF where
  foldMap f = getConst . traverse (Const . f)

instance Traversable SpringTrieF where
  traverse _ Tip = pure Tip
  traverse _ Leaf = pure Leaf
  traverse f (Pure x) = Pure <$> f x
  traverse f (Node x y) = Node <$> f x <*> f y

  sequenceA Tip = pure Tip
  sequenceA Leaf = pure Leaf
  sequenceA (Pure x) = Pure <$> x
  sequenceA (Node x y) = Node <$> x <*> y

type SpringParser = Codensity SpringParserF

type Algebra f a = f a -> a

type Coalgebra f a = a -> f a

type SpringAlgebra a = Algebra SpringTrieF a

type SpringCoalgebra a = Coalgebra SpringTrieF a

type Input = [ConditionRecord]

type OutputA = Word

type OutputB = Word

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
  return
    ConditionRecord { .. }

inputParser :: Parser Input
inputParser = many conditionRecord

emptyRecord :: ConditionRecord
emptyRecord = ConditionRecord { _row = [], _groups = [] }

nullRecord :: ConditionRecord -> Bool
nullRecord ConditionRecord{..} = null _row && null _groups

canBeDamaged :: SpringCondition -> Bool
canBeDamaged x = x == Damaged || x == Unknown

canBeOperational :: SpringCondition -> Bool
canBeOperational x = x == Operational || x == Unknown

------------ PART A ------------
final :: [(a, ConditionRecord)] -> SpringParserF a
final = maybe Fail Final . nonEmpty

run :: SpringParserF a -> ConditionRecord -> [(a, ConditionRecord)]
run (GetCondition f) cr@ConditionRecord{_row = c:cs} = run (f c) cr { _row = cs }
run (GetGroup f) cr@ConditionRecord{_groups = g:gs} = run (f g) cr { _groups = gs }
run (Fork p q) cr = run p cr ++ run q cr
run (Peek f) cr = run (f cr) cr
run (Result x p) cr = (x, cr) : run p cr
run (Final (r :| rs)) _ = r : rs
run _ _ = []

runSprings :: SpringParser a -> ConditionRecord -> [(a, ConditionRecord)]
runSprings = run . lowerCodensity

getCondition :: SpringParser SpringCondition
getCondition = Codensity GetCondition

getGroup :: SpringParser Word
getGroup = Codensity GetGroup

peek :: SpringParser ConditionRecord
peek = Codensity Peek

endOfRecord :: SpringParser ()
endOfRecord = peek >>= guard . nullRecord

satisfyCondition :: (SpringCondition -> Bool) -> SpringParser SpringCondition
satisfyCondition p = do
  x <- getCondition
  guard (p x) $> x

damaged :: SpringParser SpringCondition
damaged = satisfyCondition canBeDamaged $> Damaged

operational :: SpringParser SpringCondition
operational = satisfyCondition canBeOperational $> Operational

damageds :: SpringParser [SpringCondition]
damageds = do
  g <- getGroup
  ds <- replicateM (fromIntegral g) damaged
  os <- endOfRecord $> [] <|> operationals
  return $ ds ++ os

operationals :: SpringParser [SpringCondition]
operationals = singleton <$> operational

unknown :: SpringParser [SpringCondition]
unknown = damageds <|> operationals

springs :: SpringParser [SpringCondition]
springs = join <$> (many unknown <* endOfRecord)

parseSprings :: ConditionRecord -> [[SpringCondition]]
parseSprings = map fst . runSprings springs

countParses :: ConditionRecord -> Word
countParses = fromIntegral . length . parseSprings

partA :: Input -> OutputA
partA = getSum . foldMap (coerce . countParses)

------------ PART B ------------
takeDamaged :: ConditionRecord -> Maybe ConditionRecord
takeDamaged ConditionRecord{..} = do
  (g,gs) <- uncons _groups
  let g' = fromIntegral g
  let (ds,remnant) = take (g' + 1) &&& drop (g' + 1) $ _row
  let cr = ConditionRecord { _row = remnant, _groups = gs }
  case compare g' $ length ds of
    LT -> if all canBeDamaged (init ds) && canBeOperational (last ds)
      then return cr
      else Nothing
    EQ -> if all canBeDamaged ds then return cr else Nothing
    GT -> Nothing

takeOperational :: ConditionRecord -> Maybe ConditionRecord
takeOperational ConditionRecord{..} = do
  (x,xs) <- uncons _row
  if canBeOperational x
    then return
      ConditionRecord { _row = xs, .. }
    else Nothing

trieCoalg :: SpringCoalgebra ConditionRecord
trieCoalg cr@ConditionRecord{_groups = [], .. }
  | all canBeOperational _row = Leaf
  | otherwise = Tip
trieCoalg cr@ConditionRecord{_groups = (g:gs), .. } = case uncons _row of
  Nothing -> Tip
  Just (x,xs) -> case x of
    Damaged -> maybe Tip Pure $ takeDamaged cr
    Operational -> maybe Tip Pure $ takeOperational cr
    Unknown -> case (takeDamaged cr, takeOperational cr) of
      (Nothing,Nothing) -> Tip
      (Nothing,Just y) -> Pure y
      (Just x,Nothing) -> Pure x
      (Just x,Just y) -> Node x y
    Error -> Tip

trieAlg :: SpringAlgebra Word
trieAlg Tip = 0
trieAlg Leaf = 1
trieAlg (Pure x) = x
trieAlg (Node x y) = x + y

springHylo :: ConditionRecord -> Word
springHylo = refold trieAlg trieCoalg

-- Memoized holomorphism
memoHylo :: SpringAlgebra Word
  -> SpringCoalgebra ConditionRecord -> ConditionRecord -> Word
memoHylo alg coalg = startEvalMemo . hylo
  where
    hylo = memo $ return . alg <=< traverse hylo . coalg

memoCountParses :: ConditionRecord -> Word
memoCountParses = memoHylo trieAlg trieCoalg

duplicate :: Int -> ConditionRecord -> ConditionRecord
duplicate n ConditionRecord{..} = ConditionRecord
  {
  _row = intercalate [Unknown] $ replicate n _row,
  _groups = concat $ replicate n _groups
  }

quintuple :: ConditionRecord -> ConditionRecord
quintuple = duplicate 5

partB :: Input -> OutputB
partB = getSum . foldMap (coerce . memoCountParses . quintuple)

runDay :: R.Day
runDay = R.runDay inputParser partA partB
