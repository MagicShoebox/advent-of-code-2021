module Util.List (frequency, groupOn) where

import Control.Arrow ((&&&))
import Data.List (group, sort, groupBy)
import Data.Function (on)

-- https://stackoverflow.com/a/26372259/3491874
frequency :: Ord a => [a] -> [(a, Int)]
frequency = map (head &&& length) . group . sort

groupOn :: Eq a => (b -> a) -> [b] -> [[b]]
groupOn k = groupBy ((==) `on` k)
