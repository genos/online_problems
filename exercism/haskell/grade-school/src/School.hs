module School (School, add, empty, grade, sorted) where

import Data.List (insert)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

type School = IntMap [String]

add :: Int -> String -> School -> School
add gradeNum student = IM.insertWith (insert . head) gradeNum [student]

empty :: School
empty = IM.empty

grade :: Int -> School -> [String]
grade = IM.findWithDefault []

sorted :: School -> [(Int, [String])]
sorted = IM.toAscList
