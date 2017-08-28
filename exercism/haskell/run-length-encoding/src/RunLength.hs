module RunLength (decode, encode) where

import Control.Arrow (first, (&&&))
import Data.Char     (isNumber)
import Data.List     (group, partition)

encode :: String -> String
encode =
  concatMap (\(a, b) -> (if a /= "1" then a else "") ++ b)
    . fmap (show . length &&& (:[]) . head)
    . group

decode :: String -> String
decode = concatMap stitch . fmap (first read') . explode
  where read' i = if i == "" then 1 else read i

stitch :: (Int, Char) -> String
stitch (i, c) = replicate i c

explode :: String -> [(String, Char)]
explode [] = []
explode s  = let (i, s') = span isNumber s in (i, head s') : explode (tail s')
