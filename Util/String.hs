module Util.String (split) where

import Data.Text (unpack, pack, splitOn)

split delim = map unpack . splitOn (pack delim) . pack
