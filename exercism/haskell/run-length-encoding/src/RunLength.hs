module RunLength (decode, encode) where

import Control.Arrow (first, (&&&))
import Data.Char     (isDigit)
import Data.List     (group, partition)

encode :: String -> String
encode =
  concatMap (uncurry (++)) . fmap (show' . length &&& (:[]) . head) . group
  where show' n = if n == 1 then "" else show n

decode :: String -> String
decode [] = []
decode s  = replicate (read' i) (head s') ++ decode (tail s')
 where
  (i, s') = span isDigit s
  read' i = if i == "" then 1 else read i
