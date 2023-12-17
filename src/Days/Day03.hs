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
    _num :: Int
  } -> SchematicNumber
  deriving (Eq, Show)
makeLenses ''SchematicNumber

data SchematicVariant where
  NumberElement :: Int -> SchematicVariant
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
    _numbers :: Map.Map (Int, Int) SchematicVariant,
    _symbols :: Map.Map (Int, Int) SchematicVariant
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
toSchematicMapSingleton :: SchematicElement -> SchematicMap
toSchematicMapSingleton se@SchematicElement { _variant, _coords } = case _variant of
  NumberElement {} -> set numbers (Map.singleton _coords _variant) mempty
  SymbolElement {} -> set symbols (Map.singleton _coords _variant) mempty

isSymbolElement :: Char -> Bool
isSymbolElement = getPredicate $
  Predicate isPrint
    <> Predicate (not . isAlphaNum)
    <> Predicate (not . isSpace)
    <> Predicate (not . ('.' ==))

parseNumberElement :: Parser SchematicVariant
parseNumberElement = NumberElement . read <$> some digit

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

parseSchematicLines :: Parser [SchematicElement]
parseSchematicLines = concat <$> sepEndBy parseSchematicLine endOfLine

parseSchematics :: Parser SchematicMap
parseSchematics = foldMapOf traverse toSchematicMapSingleton <$> parseSchematicLines

readSchematics :: T.Text -> SchematicMap
readSchematics = fromRight mempty . runParser parseSchematics () "input"

runDay :: R.Day
runDay = R.runDay inputParser partA partB

inputParser :: Atto.Parser Input
inputParser = Atto.takeText

------------ PART A ------------
lengthOfSchematicItem :: SchematicVariant -> Int
lengthOfSchematicItem (NumberElement n) = length.show $ n
lengthOfSchematicItem (SymbolElement _) = 1

lookAround :: (Int, Int) -> SchematicVariant -> (Int, Int) -> SchematicVariant -> Bool
lookAround (y, x) b k a = inRange ((up, left), (down, right)) k
  where
    up = pred y
    down = succ y
    left = x - lengthOfSchematicItem a
    right = x + lengthOfSchematicItem b

hasAdjacentElement :: Map.Map (Int, Int) SchematicVariant ->
  (Int, Int) -> SchematicVariant -> Bool
hasAdjacentElement m k v = iany (lookAround k v) m

partNumberSum :: SchematicMap -> Int
partNumberSum schematics = au (_Wrapping Sum)
  (foldMapOf $ numbers . itraversed
    . ifiltered (schematics ^. symbols . to hasAdjacentElement)
    . indexing _NumberElement)
  schematics

partA :: Input -> OutputA
partA = partNumberSum . readSchematics

------------ PART B ------------
getAdjacentElements :: Map.Map (Int, Int) SchematicVariant ->
  (Int, Int) -> SchematicVariant -> [SchematicVariant]
getAdjacentElements m k v = m ^.. itraversed . ifiltered (lookAround k v)

getGearAssocs :: SchematicMap -> [((Int, Int), SchematicVariant)]
getGearAssocs = toListOf $ symbols . to itoList . traversed . filtered ((SymbolElement '*' ==).snd)

getAdjacentNumbers :: SchematicMap -> [((Int, Int), SchematicVariant)] ->
  [((Int, Int), [Int])]
getAdjacentNumbers schematics = over mapped $ (,)
  <$> fst
  <*> catMaybes . map (preview _NumberElement)
    . uncurry (schematics ^. numbers . to getAdjacentElements)

getGearRatioSum :: [((Int, Int), [Int])] -> Int
getGearRatioSum = au (_Wrapping Sum)
  (foldMapOf $ traverse . filtered ((2 ==).length.snd) . _2 . to product)

partB :: Input -> OutputB
partB = getGearRatioSum . (getAdjacentNumbers <*> getGearAssocs) . readSchematics
