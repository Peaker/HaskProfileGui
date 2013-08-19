{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Control.Monad
import Data.List (intercalate)
import Data.Monoid (Monoid(..))
import ListUtils (sortOn)
import Parser (CostCentre(..), CostCentreData(..), TimeAlloc(..))
import Parser (parser)
import System.Environment (getArgs)
import TextUtils (showText)
import qualified Data.Attoparsec.Text.Lazy as P
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
-- import qualified GUI
-- import qualified Graphics.UI.WX as WX
import qualified Parser

main :: IO ()
main = do
  [filename] <- getArgs
  parseRes <- P.parse parser <$> TLIO.readFile filename
  res <-
    case parseRes of
    P.Fail remaining contexts err ->
      fail $
      "Parse error near " ++ (show . TL.intercalate " " . take 10 . TL.splitOn " ") remaining ++
      " in (" ++ intercalate "." contexts ++ "): " ++ show err
    P.Done _ res -> return res
  let
    pair cc =
      ( Parser.ccFullyQualifiedName cc
      , (ccIndividual cc, ccInherited cc)
      )
    accumulatedCosts =
      take 800 .
      sortOn (negate . timePercent . snd . snd) .
      Map.toList .
      Map.fromListWith mappend .
      map (pair . ccData) $ Parser.allCostCentres res
  forM_ accumulatedCosts $ \(fqName, (individual, inherited)) ->
    TIO.putStrLn $ T.concat [fqName, ":", showTime inherited, "(", showTime individual, ")"]
  -- WX.start $ GUI.gui res
  where
    showTime = showText . timePercent
