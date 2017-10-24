module Scrabble (scoreLetter, scoreWord) where

import Data.Char       (toUpper)
import Data.Map.Strict (Map, findWithDefault, fromList)

table :: Map Char Integer
table = fromList
  [ (c, i)
  | (cs, i) <-
    [ (['A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T'], 1)
    , (['D', 'G']                                        , 2)
    , (['B', 'C', 'M', 'P']                              , 3)
    , (['F', 'H', 'V', 'W', 'Y']                         , 4)
    , (['K']                                             , 5)
    , (['J', 'X']                                        , 8)
    , (['Q', 'Z']                                        , 10)
    ]
  , c <- cs
  ]

scoreLetter :: Char -> Integer
scoreLetter = flip (findWithDefault 0) table . toUpper

scoreWord :: String -> Integer
scoreWord = sum . fmap scoreLetter
