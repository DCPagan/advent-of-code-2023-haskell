{-# LANGUAGE TemplateHaskell #-}
module Days.Day08 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative
import Control.Lens
import Data.List
import Data.HashMap.Strict (HashMap, fromList, keys)
import Data.Maybe
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

------------ TYPES ------------
data Turn where
  L :: Turn
  R :: Turn
  deriving (Eq)

instance Show Turn where
  show L = "L"
  show R = "R"
  showList [] s = s
  showList (d:ds) s = show d ++ showList ds s

data Directions where
  Directions :: {
    _turns :: [Turn],
    _network :: HashMap String (String, String)
  } -> Directions
  deriving (Eq, Show)
makeLenses ''Directions

instance Semigroup Directions where
  a <> b = Directions {
    _turns = _turns a <> _turns b,
    _network = _network a <> _network b
  }

instance Monoid Directions where
  mempty = Directions mempty mempty

type Input = Directions

type OutputA = Int

type OutputB = Int

------------ PARSER ------------
parseTurn :: Parser Turn
parseTurn = L <$ char 'L' <|> R <$ char 'R'

parseTurns :: Parser [Turn]
parseTurns = many parseTurn <* endOfLine

parseEntry :: Parser (String, (String, String))
parseEntry = do
  key <- some letter
  skipSpace
  char '='
  skipSpace
  char '('
  left <- some letter
  char ','
  skipSpace
  right <- some letter
  char ')'
  endOfLine
  return (key, (left, right))

parseEntries :: Parser (HashMap String (String, String))
parseEntries = fromList <$> many parseEntry

parseDirections :: Parser Directions
parseDirections = do
  _turns <- parseTurns
  endOfLine
  _network <- parseEntries
  return Directions { _turns, _network }

inputParser :: Parser Input
inputParser = parseDirections

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PART A ------------
navigate :: HashMap String (String, String) -> (String -> Bool) ->
  [Turn] -> String -> [String]
navigate _ _ [] _ = []
navigate m p (t:ts) k
  | p k = []
  | otherwise = k' : navigate m p ts k'
  where
    k' = m ^?! ix k . lr
    lr = case t of
      L -> _1
      R -> _2

partA :: Input -> OutputA
partA Directions { _turns, _network } = length $
  navigate _network ("ZZZ" ==) (cycle _turns) "AAA"

------------ PART B ------------
navigateGhosts :: Directions -> [[String]]
navigateGhosts Directions { _turns, _network } =
  map (navigate _network (('Z' ==) . last) (cycle _turns))
  . filter (('A' ==) . last) . keys $ _network

partB :: Input -> OutputB
partB = foldr (lcm . length) 1 . navigateGhosts
