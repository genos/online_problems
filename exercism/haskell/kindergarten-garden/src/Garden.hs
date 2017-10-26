module Garden
    ( Plant (..)
    , defaultGarden
    , garden
    , lookupPlants
    ) where

import           Data.List       (lines, sort)
import           Data.List.Split (chunksOf, splitOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

data Plant   = Clover | Grass | Radishes | Violets deriving (Eq, Show)
type Student = String
type Diagram = String
type Garden  = Map Student [Plant]

plant :: Char -> Plant
plant 'C' = Clover
plant 'G' = Grass
plant 'R' = Radishes
plant 'V' = Violets
plant c   = error $! "Invalid plant encoding: " ++ [c]

allStudents :: [Student]
allStudents = splitOn
  ","
  "Alice,Bob,Charlie,David,Eve,Fred,Ginny,Harriet,Ileana,Joseph,Kincaid,Larry"

defaultGarden :: Diagram -> Garden
defaultGarden = garden allStudents

-- Assumes diagram is properly formatted, which is probably dangerous
garden :: [Student] -> Diagram -> Garden
garden students diagram = M.fromList $! zip (sort students) grouped
 where
  [first, second] = fmap (chunksOf 2 . fmap plant) $! lines diagram
  grouped         = zipWith (++) first second

lookupPlants :: Student -> Garden -> [Plant]
lookupPlants = M.findWithDefault []
