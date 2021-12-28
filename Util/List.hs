module Util.List (frequency) where

import Control.Arrow ((&&&))
import Data.List (group, sort)

-- https://stackoverflow.com/a/26372259/3491874
frequency :: Ord a => [a] -> [(Int, a)]
frequency = map (length &&& head) . group . sort
