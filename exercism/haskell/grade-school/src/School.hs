module School (School, add, empty, grade, sorted) where

import Data.Foldable (foldl')
import Data.List (insert)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type School = Map Int [String]

add :: Int -> String -> School -> School
add gradeNum student = M.insertWith (foldl' $ flip insert) gradeNum [student]

empty :: School
empty = M.empty

grade :: Int -> School -> [String]
grade = M.findWithDefault []

sorted :: School -> [(Int, [String])]
sorted = M.assocs
