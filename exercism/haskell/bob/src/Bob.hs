module Bob (responseFor) where

import Data.Char (isAlpha, isSpace, toUpper)
import Data.List (dropWhileEnd)

responseFor :: String -> String
responseFor = reply . trim
  where
    reply s | null s || all isSpace s              = "Fine. Be that way!"
            | any isAlpha s && s == fmap toUpper s = "Whoa, chill out!"
            | last s == '?'                        = "Sure."
            | otherwise                            = "Whatever."
    trim = dropWhileEnd isSpace . dropWhile isSpace
