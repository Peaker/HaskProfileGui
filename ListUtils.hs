module ListUtils (sortOn) where

import Data.List (sortBy)
import Data.Ord (comparing)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = sortBy . comparing
