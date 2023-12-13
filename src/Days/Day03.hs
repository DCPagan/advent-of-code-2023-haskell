{-# LANGUAGE TemplateHaskell #-}
module Days.Day03 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative
import Control.Lens
import qualified Data.Attoparsec.Text as Atto
import Data.Char
import Data.Either
import Data.Functor.Contravariant
import Data.Ix (inRange)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Void
import Text.Parsec hiding ((<|>), many, some)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Text (Parser)

import qualified Program.RunDay as R (runDay, Day)
{- ORMOLU_ENABLE -}

------------ TYPES ------------
data SchematicNumber where
  SchematicNumber :: {
    _num :: Int,
    _len :: Int
  } -> SchematicNumber
  deriving (Eq, Show)
makeLenses ''SchematicNumber

data SchematicVariant where
  NumberElement :: SchematicNumber -> SchematicVariant
  SymbolElement :: Char -> SchematicVariant
  deriving (Eq, Show)
makePrisms ''SchematicVariant

data SchematicElement where
  SchematicElement :: {
    _variant :: SchematicVariant,
    _coords :: (Int, Int)
  } -> SchematicElement
  deriving (Eq, Show)
makeLenses ''SchematicElement

data SchematicMap where
  SchematicMap :: {
    _numbers :: Map.Map (Int, Int) SchematicNumber,
    _symbols :: Map.Map (Int, Int) Char
  } -> SchematicMap
  deriving (Eq, Show)
makeLenses ''SchematicMap

instance Semigroup SchematicMap where
  a <> b = SchematicMap {
    _numbers = _numbers a <> _numbers b,
    _symbols = _symbols a <> _symbols b
  }

instance Monoid SchematicMap where
  mempty = SchematicMap {
    _numbers = mempty,
    _symbols = mempty
  }

type Input = T.Text

type OutputA = Int

type OutputB = Int

------------ PARSER ------------
isSymbolElement :: Char -> Bool
isSymbolElement = getPredicate $
  Predicate isPrint
    <> Predicate (not . isAlphaNum)
    <> Predicate (not . isSpace)
    <> Predicate (not . ('.' ==))

parseNumberElement :: Parser SchematicVariant
parseNumberElement = do
  digits <- some digit
  return $ NumberElement SchematicNumber {
    _num = read digits,
    _len = length digits
  }

parseSymbolElement :: Parser SchematicVariant
parseSymbolElement = SymbolElement <$> satisfy isSymbolElement

parseSchematicVariant :: Parser SchematicVariant
parseSchematicVariant = parseNumberElement <|> parseSymbolElement

parseSchematicElement :: Parser SchematicElement
parseSchematicElement = do
  _coords <- (,) <$> sourceLine <*> sourceColumn <$> getPosition
  _variant <- parseSchematicVariant
  return SchematicElement { _variant, _coords }

parseSchematicLine :: Parser [SchematicElement]
parseSchematicLine = many $
  between (many (char '.')) (many (char '.')) parseSchematicElement

parseSchematics :: Parser [SchematicElement]
parseSchematics = concat <$> sepEndBy parseSchematicLine endOfLine

parseSchematicMap :: Parser SchematicMap
parseSchematicMap = foldMapOf traverse toSchematicMapSingleton <$> parseSchematics

runDay :: R.Day
runDay = R.runDay inputParser partA partB

inputParser :: Atto.Parser Input
inputParser = Atto.takeText

------------ PART A ------------
toSchematicMapSingleton :: SchematicElement -> SchematicMap
toSchematicMapSingleton se@SchematicElement { _variant, _coords } = case _variant of
  NumberElement n -> set numbers (Map.singleton _coords n) mempty
  SymbolElement c -> set symbols (Map.singleton _coords c) mempty

lookLeft :: ((Int, Int), SchematicNumber) -> (Int, Int) -> Bool
lookLeft (k, _) = (over _2 pred k ==)

lookRight :: ((Int, Int), SchematicNumber) -> (Int, Int) -> Bool
lookRight (k, SchematicNumber { _len }) = (over _2 (_len +) k ==)

lookUp :: ((Int, Int), SchematicNumber) -> (Int, Int) -> Bool
lookUp ((y, x), SchematicNumber { _len }) = inRange ((u, l), (u, r))
  where
    u = pred y
    l = pred x
    r = x + _len

lookDown :: ((Int, Int), SchematicNumber) -> (Int, Int) -> Bool
lookDown ((y, x), SchematicNumber { _len }) = inRange ((d, l), (d, r))
  where
    d = succ y
    l = pred x
    r = x + _len

lookAround :: ((Int, Int), SchematicNumber) -> (Int, Int) -> Bool
lookAround = (getAny .) .
  ((Any .) . lookUp
    <> (Any .) . lookDown
    <> (Any .) . lookLeft
    <> (Any .) . lookRight)

hasAdjacentElement :: Map.Map (Int, Int) a -> (Int, Int) -> SchematicNumber -> Bool
hasAdjacentElement m k n = iany (const . lookAround (k, n)) m

numbersAdjacentToSymbol :: SchematicMap -> Map.Map (Int, Int) SchematicNumber
numbersAdjacentToSymbol m@SchematicMap { _numbers, _symbols }
  = Map.filterWithKey (hasAdjacentElement _symbols) _numbers

partA :: Input -> OutputA
partA = getSum . foldMapOf (traverse . num) Sum
  . numbersAdjacentToSymbol . fromRight mempty
  . runParser parseSchematicMap () "input"

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
