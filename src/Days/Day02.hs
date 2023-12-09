{-# LANGUAGE TemplateHaskell #-}
module Days.Day02 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative ((<|>), many)
import Control.Lens
import Data.Attoparsec.Text
import Data.Foldable (fold)
import Data.Function (on)
import Data.Functor.Contravariant
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T

import qualified Program.RunDay as R (runDay, Day)
{- ORMOLU_ENABLE -}

------------ TYPES ------------
data Draw where
  Draw :: {
    _red :: Word,
    _green :: Word,
    _blue :: Word
  } -> Draw
  deriving (Eq)
makeLenses ''Draw

data Game where
  Game :: {
    _serialId :: Word,
    _draws :: [Draw]
  } -> Game
  deriving (Eq)
makeLenses ''Game

instance Show Draw where
  show draw = intercalate ", " . filter (not . null) $ map attr attrs <*> pure draw
    where
      attr (field, name) draw = if value > 0
        then show value <> " " <> name else ""
        where value = field draw
      attrs = [(_red, "red"), (_green, "green"), (_blue, "blue")]
  showList draws rest = (intercalate "; " . map show . filter (/= mempty) $ draws) <> rest

instance Semigroup Draw where
  a <> b = Draw {
    _red = _red a + _red b,
    _green = _green a + _green b,
    _blue = _blue a + _blue b
  }

instance Monoid Draw where
  mempty = Draw {
    _red = 0,
    _green = 0,
    _blue = 0
  }

instance Show Game where
  show game = "Game " <> (show . _serialId $ game) <> ": "
    <> (show . _draws $ game)
  showList games rest = (intercalate "\n" . map show $ games) <> rest

type Input = [Game]

type OutputA = Word

type OutputB = Word

------------ PARSER ------------
parseColor :: Parser Draw
parseColor = do
  n <- decimal
  skipSpace
  field <- red <$ string "red" <|> green <$ string "green" <|> blue <$ string "blue" 
  return $ set field n mempty

parseDraw :: Parser Draw
parseDraw = fold <$> sepBy parseColor (skipSpace >> char ',' >> skipSpace)

parseDraws :: Parser [Draw]
parseDraws = sepBy parseDraw (skipSpace >> char ';' >> skipSpace)

parseGame :: Parser Game
parseGame = do
  string "Game"
  skipSpace
  i <- decimal
  skipSpace
  char ':'
  skipSpace
  ds <- parseDraws
  return $ Game { _serialId = i, _draws = ds }

parseGames :: Parser [Game]
parseGames = sepBy parseGame endOfLine

inputParser :: Parser [Game]
inputParser = parseGames

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PART A ------------
limit :: Draw
limit = Draw {
  _red = 12,
  _green = 13,
  _blue = 14
}

drawLimit :: Draw -> Draw -> Bool
drawLimit = getPredicate . foldMap
  ((Predicate .) . on (>=) . view) [red, green, blue]

filterByDrawLimit :: Draw -> [Game] -> [Game]
filterByDrawLimit = filter . allOf (draws . traverse) . drawLimit

partA :: Input -> OutputA
partA = getSum . foldMapOf (traverse . serialId) Sum
  . filterByDrawLimit limit

------------ PART B ------------
maxColors :: Draw -> Draw -> Draw
maxColors a b = Draw {
  _red = on max _red a b,
  _green = on max _green a b,
  _blue = on max _blue a b
}

maximumColors :: Game -> Draw
maximumColors = foldrOf (draws . traverse) maxColors mempty

colorPower :: Draw -> Word
colorPower = getProduct . foldMap ((Product .) . view) [red, green, blue]

partB :: Input -> OutputB
partB = getSum . foldMapOf traverse (Sum . colorPower . maximumColors)
