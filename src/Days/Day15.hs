{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Days.Day15 (runDay) where

import Control.Arrow
import Control.Monad.Combinators
import Control.Monad.Primitive
import Control.Lens
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.ST
import Data.Attoparsec.Text (Parser, char, endOfLine, notChar)
import Data.Char
import Data.Coerce
import Data.Functor
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Word

import qualified Program.RunDay as R (Day,runDay)

import qualified Text.ParserCombinators.ReadP as R
import qualified Text.ParserCombinators.ReadPrec as RP
import Text.Read

import qualified Util.Util as U

------------ TYPES ------------
data HolidayLens where
  HolidayLens :: {
    _lensLabel :: String,
    _focalLength :: Word
  } -> HolidayLens
  deriving (Eq, Ord)

makeLenses ''HolidayLens

instance Read HolidayLens where
  readPrec = lift $ do
    _lensLabel <- R.munch (',' /=)
    R.char '='
    _focalLength <- read <$> R.munch isDigit
    return HolidayLens { .. }

  readListPrec = sepBy readPrec $ RP.lift $ R.char ','

instance Show HolidayLens where
  show HolidayLens { .. } = _lensLabel ++ "=" ++ show _focalLength

  showList ls = (intercalate "," (show <$> ls) ++)

data HolidayOperation where
  Take :: HolidayOperation
  Put :: Word -> HolidayOperation
  deriving (Eq, Ord)

makePrisms ''HolidayOperation

instance Read HolidayOperation where
  readPrec = lift $ take <|> put
    where
      take = R.char '-' $> Take
      put = R.char '=' >> Put . read <$> R.munch isDigit

instance Show HolidayOperation where
  show Take = "-"
  show (Put x) = '=' : show x

data HolidayStep where
  HolidayStep :: {
    _stepLabel :: String,
    _stepOp :: HolidayOperation
  } -> HolidayStep
  deriving (Eq, Ord)

makeLenses ''HolidayStep

instance Read HolidayStep where
  readPrec = do
    _stepLabel <- lift $ R.munch isAlpha
    _stepOp <- readPrec
    return HolidayStep { .. }

  readListPrec = sepBy readPrec $ RP.lift $ R.char ','

instance Show HolidayStep where
  show HolidayStep { .. } = _stepLabel ++ show _stepOp

  showList ls = (intercalate "," (show <$> ls) ++)

type HashMap = Array Word8 [HolidayLens]

type Input = [String]

type OutputA = Word

type OutputB = Word

------------ PARSER ------------
holidayStep :: Parser String
holidayStep = some $ skipMany endOfLine *> notChar ','

holidaySteps :: Parser [String]
holidaySteps = sepBy holidayStep $ char ','

inputParser :: Parser Input
inputParser = holidaySteps

------------ PART A ------------
holidayHash :: String -> Word8
holidayHash = ($ 0) . auf (_Wrapping Dual . _Wrapping Endo) foldMap
  (((17 *) .) . (+) . fromIntegral . fromEnum)

partA :: Input -> OutputA
partA = auf (_Wrapping Sum) foldMap (fromIntegral . holidayHash)

------------ PART B ------------
initialHashMap :: HashMap
initialHashMap = array (minBound, maxBound) $ fmap (, []) (enumFrom 0)

hasLabel :: String -> HolidayLens -> Bool
hasLabel label HolidayLens { .. } = label == _lensLabel

executeHolidayStep :: (PrimMonad m, MArray a [HolidayLens] m)
  => a Word8 [HolidayLens]
  -> HolidayStep
  -> m (a Word8 [HolidayLens])
executeHolidayStep a HolidayStep { _stepLabel = _lensLabel, .. } = do
  case _stepOp of
    Take -> modifyArray a hash
      $ uncurry (++) . second (^. _tail) . break (hasLabel _lensLabel)
    Put _focalLength -> do
      hlenses <- readArray a hash
      if any (hasLabel _lensLabel) hlenses
        then modifyArray a hash
          $ uncurry (++) . second (_head . focalLength .~ _focalLength)
            . break (hasLabel _lensLabel)
        else modifyArray a hash (`snoc` hlens)
          where
            hlens = HolidayLens { .. }
  return a
  where
    hash = holidayHash _lensLabel

focusingPower :: Word8 -> [HolidayLens] -> Word
focusingPower i = coerce . ifoldMap 
  (\j l -> Sum
    $ succ (fromIntegral i)
    * succ (fromIntegral j)
    * l ^. focalLength)

executeHASHMAP :: (PrimMonad m, MArray a [HolidayLens] m, Traversable t)
  => t HolidayStep
  -> m (a Word8 [HolidayLens])
executeHASHMAP t = do
  a <- thaw initialHashMap
  mapM_ (executeHolidayStep a) t
  return a

partB :: Input -> OutputB
partB t = coerce . ifoldMap ((Sum .) . focusingPower) $ runSTArray
  $ executeHASHMAP $ read <$> t

runDay :: R.Day
runDay = R.runDay inputParser partA partB
