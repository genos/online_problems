module RunLength (decode, encode) where

import Control.Arrow (first, (&&&))
import Data.Char     (isDigit)
import Data.List     (group, partition)

encode :: String -> String
encode =
  concatMap (uncurry (++)) . fmap (show' . length &&& (:[]) . head) . group
  where show' n = if n == 1 then "" else show n

decode :: String -> String
decode = concatMap (uncurry replicate) . fmap (first read') . explode
  where read' i = if i == "" then 1 else read i

explode :: String -> [(String, Char)]
explode [] = []
explode s  = (i, head s') : explode (tail s') where (i, s') = span isDigit s
