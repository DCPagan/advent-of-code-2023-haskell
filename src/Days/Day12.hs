{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use infix" #-}

module Days.Day12 (runDay) where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.Codensity
import           Control.Lens hiding (cons, snoc, uncons, unsnoc)
import           Control.Monad
import           Data.Attoparsec.Text hiding (Fail, take)
import           Data.Coerce
import           Data.Fix
import           Data.Function
import           Data.Functor
import           Data.List
import           Data.List.Extra
import           Data.List.NonEmpty (NonEmpty((:|)), nonEmpty, toList)
import           Data.Maybe
import           Data.Monoid
import           Data.Void
import           GHC.Read
import qualified Text.ParserCombinators.ReadPrec as RP
import qualified Program.RunDay as R (runDay, Day)
import qualified Util.Util as U
import           Data.Text.Internal.Fusion.Types (RS(RS0))
import           Data.Bifunctor.TH (deriveBifunctor)

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

data SpringParserF a where
  Fail :: SpringParserF a
  GetCondition :: (SpringCondition -> SpringParserF a) -> SpringParserF a
  GetGroup :: (Word -> SpringParserF a) -> SpringParserF a
  Bifurcate :: SpringParserF a -> SpringParserF a -> SpringParserF a
  Look :: (ConditionRecord -> SpringParserF a) -> SpringParserF a
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
  Bifurcate p q >>= k = Bifurcate (p >>= k) (q >>= k)
  Look f >>= k = Look (f >=> k)
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
  Final r <|> p = Look $ Final . maybe r (r <>) . nonEmpty . run p
  p <|> Final r = Look $ Final . maybe r (<> r) . nonEmpty . run p
  Look f <|> Look g = Look $ uncurry (<|>) <<< f &&& g
  Look f <|> p = Look $ (<|> p) . f
  p <|> Look f = Look $ (p <|>) . f
  GetCondition f <|> GetGroup g = Bifurcate (GetCondition f) (GetGroup g)
  GetGroup g <|> GetCondition f = Bifurcate (GetCondition f) (GetGroup g)
  Bifurcate p q <|> Bifurcate r s = Bifurcate (Bifurcate p r) (Bifurcate q s)
  Bifurcate p q <|> r = Bifurcate (p <|> r) (q <|> r)
  r <|> Bifurcate p q = Bifurcate (r <|> p) (r <|> q)

instance MonadPlus SpringParserF

instance MonadFail SpringParserF where
  fail = const Fail

type SpringParser = Codensity SpringParserF

instance (Show a) => Show (SpringParserF a) where
  show Fail = "Fail"
  show (GetCondition _) = "GetCondition"
  show (GetGroup _) = "GetGroup"
  show (Bifurcate p q) = "Bifurcate " ++ show p ++ " " ++ show q
  show (Look _) = "Look"
  show (Result x p) = "Result: " ++ show x ++ " " ++ show p
  show (Final r) = "Final: " ++ show r

type Input = [ConditionRecord]

type OutputA = Word

type OutputB = Word

------------ PARSER ------------
emptyRecord :: ConditionRecord
emptyRecord = ConditionRecord { _row = [], _groups = [] }

final :: [(a, ConditionRecord)] -> SpringParserF a
final = maybe Fail Final . nonEmpty

run :: SpringParserF a -> ConditionRecord -> [(a, ConditionRecord)]
run (GetCondition f) cr@ConditionRecord { _row = c:cs } =
  run (f c) cr { _row = cs }
run (GetGroup f) cr@ConditionRecord { _groups = g:gs } =
  run (f g) cr { _groups = gs }
run (Bifurcate p q) cr = run p cr ++ run q cr
run (Look f) cr = run (f cr) cr
run (Result x p) cr = (x, cr):run p cr
run (Final (r :| rs)) _ = r:rs
run _ _ = []

runSprings :: SpringParser a -> ConditionRecord -> [(a, ConditionRecord)]
runSprings = run . lowerCodensity

getCondition :: SpringParser SpringCondition
getCondition = Codensity GetCondition

getGroup :: SpringParser Word
getGroup = Codensity GetGroup

look :: SpringParser ConditionRecord
look = Codensity Look

satisfyCondition :: (SpringCondition -> Bool) -> SpringParser SpringCondition
satisfyCondition p = do
  x <- getCondition
  if p x
    then return x
    else empty

eof :: SpringParser ()
eof = do
  ConditionRecord { .. } <- look
  unless (null _row) empty

getDamaged :: SpringParser SpringCondition
getDamaged = satisfyCondition (\x -> x == Damaged || x == Unknown) $> Damaged

getOperational :: SpringParser SpringCondition
getOperational = satisfyCondition (\x -> x == Operational || x == Unknown)
  $> Operational

getDamageds :: SpringParser [SpringCondition]
getDamageds = do
  g <- getGroup
  ds <- replicateM (fromIntegral g) getDamaged
  os <- eof $> [] <|> getOperationals
  return $ ds ++ os

getOperationals :: SpringParser [SpringCondition]
getOperationals = singleton <$> getOperational

getUnknowns :: SpringParser [SpringCondition]
getUnknowns = getDamageds <|> getOperationals

getAllSprings :: SpringParser [SpringCondition]
getAllSprings = join <$> many getUnknowns

parseSprings :: ConditionRecord -> [[SpringCondition]]
parseSprings =
  map fst . filter ((emptyRecord ==) . snd) . runSprings getAllSprings

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
countParses :: ConditionRecord -> Word
countParses = fromIntegral . length . parseSprings

partA :: Input -> OutputA
partA = getSum . foldMap (coerce . countParses)

------------ PART B ------------
duplicate :: Int -> ConditionRecord -> ConditionRecord
duplicate n =
  row %~ intercalate [Unknown] . replicate n <<< groups %~ concat . replicate n

quintuple :: ConditionRecord -> ConditionRecord
quintuple = duplicate 5

partB :: Input -> OutputB
-- partB = error "Unimplemented"
partB = partA <<< fmap quintuple

runDay :: R.Day
runDay = R.runDay inputParser partA partB
