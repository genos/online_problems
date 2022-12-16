{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Attoparsec.Text as A
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text.IO as T

type Name = Text
data Valve = Valve
    { _flow :: {-# UNPACK #-} !Int
    , _open :: !Bool
    , _tunnels :: [Name]
    }
    deriving (Show)

$(makeLenses ''Valve)

readValves :: Text -> Map Name Valve
readValves = either (error "Bad Parse") M.fromList . parseOnly (pValve `sepBy1'` endOfLine)
  where
    pValve = pair <$> ("Valve " *> pName) <*> pFlow <*> pTunnels
    pName = A.take 2
    pFlow = " has flow rate=" *> decimal
    pTunnels = "; " *> choice ["tunnel leads to valve ", "tunnels lead to valves "] *> (pName `sepBy'` ", ")
    pair n i ns = (n, Valve i False ns)

main :: IO ()
main = do
    input <- readValves <$> T.readFile "test.txt"
    print input
