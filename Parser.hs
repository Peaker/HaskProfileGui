{-# OPTIONS -Wall -O2 -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser (TimeAlloc(..), CostCentre(..), CostCentreData(..), parser) where

import Control.Applicative
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad.Trans.State (State, runState)
import qualified Control.Monad.Trans.State as State
import qualified Data.Attoparsec.Text.Lazy as P
import qualified Data.Text as T

anyLine :: P.Parser T.Text
anyLine = P.takeTill P.isEndOfLine <* P.endOfLine

token :: P.Parser T.Text
token = P.takeWhile (not . P.isHorizontalSpace)

manyTill :: Alternative f => f a -> f b -> f ([a], b)
manyTill x final = go
  where
    go = ((,) [] <$> final) <|> (add <$> x <*> go)
    add res = _1 %~ (res :)

data TimeAlloc = TimeAlloc { timePercent :: Double, allocPercent :: Double }
  deriving (Show)

data CostCentreData = CostCentreData
  { ccName :: T.Text
  , ccModule :: T.Text
  , ccNo :: Int
  , ccEntries :: Int
  , ccIndividual :: TimeAlloc
  , ccInherited :: TimeAlloc
  } deriving (Show)

data CostCentre = CostCentre
  { ccData :: CostCentreData
  , ccChildren :: [CostCentre]
  } deriving (Show)

data IndentedCostCentre = IndentedCostCentre
  { _iccIndent :: Int
  , _iccCC :: CostCentreData
  } deriving (Show)

horizontalSpace :: P.Parser Char
horizontalSpace = P.satisfy P.isHorizontalSpace

costCenterLine :: P.Parser IndentedCostCentre
costCenterLine =
  -- name module no entries individual:(time alloc) inherited:(time alloc)
  IndentedCostCentre <$>
    -- indent:
    (length <$> many horizontalSpace) <*>
    (CostCentreData <$>
     -- name
     token <* P.skipSpace <*>
     -- module
     token <* P.skipSpace <*>
     -- no
     P.decimal <* P.skipSpace <*>
     -- entries
     P.decimal <* P.skipSpace <*>
     -- individual:
     timeAlloc <* P.skipSpace <*>
     -- inherited:
     timeAlloc <* P.endOfLine)
  where
    timeAlloc = TimeAlloc <$> P.double <* P.skipSpace <*> P.double

fromIndented :: [IndentedCostCentre] -> [CostCentre]
fromIndented xs =
  case runState (getChildren (-1)) xs of
    (res, []) -> res
    _ -> error "Indent level < 0??"

getChildren :: Int -> State [IndentedCostCentre] [CostCentre]
getChildren i = do
  s <- State.get
  case s of
    [] -> return []
    IndentedCostCentre j cc : rest ->
      if j <= i
      then return []
      else do
        State.put rest
        children <- getChildren j
        siblings <- getChildren i
        return $ CostCentre cc children : siblings

parser :: P.Parser [CostCentre]
parser = do
  (_, mainCostCenter) <- anyLine `manyTill` costCenterLine
  ccs <- many costCenterLine
  P.endOfInput
  return . fromIndented $ mainCostCenter : ccs
