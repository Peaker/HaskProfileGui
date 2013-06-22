{-# OPTIONS -Wall -O2 #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Data.List (intercalate)
import Parser (parser)
import System.Environment (getArgs)
import qualified Data.Attoparsec.Text.Lazy as P
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TIO

main :: IO ()
main = do
  [filename] <- getArgs
  parseRes <- P.parse parser <$> TIO.readFile filename
  res <-
    case parseRes of
    P.Fail remaining contexts err ->
      fail $
      "Parse error near " ++ (show . TL.intercalate " " . take 10 . TL.splitOn " ") remaining ++
      " in " ++ intercalate "." contexts ++ ": " ++ show err
    P.Done _ res -> return res
  print res
