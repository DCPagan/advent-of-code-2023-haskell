{-# LANGUAGE GADTs #-}

module Days.Day15 (runDay) where

import Control.Monad.Combinators
import Control.Lens
import Data.Attoparsec.Text (Parser, char, endOfLine, notChar)
import Data.Coerce
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Void
import Data.Word

import qualified Program.RunDay as R (Day,runDay)

import qualified Util.Util as U

------------ TYPES ------------
type Input = [String]

type OutputA = Word

type OutputB = Void

------------ PARSER ------------
step :: Parser String
step = some $ skipMany endOfLine *> notChar ','

steps :: Parser [String]
steps = sepBy step $ char ','

inputParser :: Parser Input
inputParser = steps

------------ PART A ------------
holidayHash :: String -> Word8
holidayHash = ($ 0) . auf (_Wrapping Dual . _Wrapping Endo) foldMap
  (((17 *) .) . (+) . fromIntegral . fromEnum)

partA :: Input -> OutputA
partA = auf (_Wrapping Sum) foldMap (fromIntegral . holidayHash)

------------ PART B ------------
partB :: Input -> OutputB
partB = undefined

runDay :: R.Day
runDay = R.runDay inputParser partA partB
