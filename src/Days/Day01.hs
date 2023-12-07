module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative ((<|>), many)
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text
import Data.Char (digitToInt, isDigit)
import Data.Either (fromRight)
import Data.List
import Data.Maybe
import qualified Data.Text as T

import qualified Program.RunDay as R (runDay, Day)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = takeText

------------ TYPES ------------
type Input = T.Text

type OutputA = Int

type OutputB = Int

------------ PART A ------------
calibration :: [Int] -> Int
calibration = (+) <$> (10 *) . head <*> last

finalCalibration :: (T.Text -> [Int]) -> T.Text -> Int
finalCalibration getDigits = sum . map (calibration . getDigits) . T.lines

partA :: Input -> OutputA
partA = finalCalibration (map digitToInt . filter isDigit . T.unpack)

------------ PART B ------------
nominalNumber :: Parser Int
nominalNumber = 1 <$ string "one"
  <|> 2 <$ string "two"
  <|> 3 <$ string "three"
  <|> 4 <$ string "four"
  <|> 5 <$ string "five"
  <|> 6 <$ string "six"
  <|> 7 <$ string "seven"
  <|> 8 <$ string "eight"
  <|> 9 <$ string "nine"

numbers :: Parser [Int]
numbers = catMaybes <$> many (
  fmap Just (lookAhead (nominalNumber <|> digitToInt <$> digit) <* anyChar)
    <|> Nothing <$ anyChar)

partB :: Input -> OutputB
partB = finalCalibration (fromRight [] . parseOnly numbers)
