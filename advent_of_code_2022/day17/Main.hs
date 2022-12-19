-- help from https://work.njae.me.uk/2022/12/19/advent-of-code-2022-day-17/
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.Set (Set)
import qualified Data.Set as S
import Linear.V2
import Linear.Vector

type Pos = V2 Int
type Rock = Set Pos
type Jet = Pos
type Chamber = Set Pos

rockCycle :: [Rock]
rockCycle =
    cycle $
        ["####", ".#.\n###\n.#.", "..#\n..#\n###", "#\n#\n#\n#", "##\n##"]
            <&> \s -> S.fromList [V2 x y | (y, l) <- zip [0 ..] (reverse $ lines s), (x, c) <- zip [0 ..] l, c == '#']

allows :: Chamber -> Set Pos -> Bool
allows c = none (\p -> (p ^. _x < 0) || (p ^. _x > 6) || (p ^. _y) < 0 || p `S.member` c)

readJets :: String -> [Jet]
readJets = fmap $ \case
    '<' -> negate $ unit _x
    '>' -> unit _x
    c -> error $ "Unexpected character: " <> show c

data State = State
    { _chamber :: !Chamber
    , _units :: {-# UNPACK #-} !Int
    , _height :: {-# UNPACK #-} !Int
    , _jets :: [Jet]
    , _rocks :: [Rock]
    }
makeLenses ''State

setup :: [Jet] -> State
setup js = State S.empty 0 (-1) (cycle js) rockCycle

push :: Chamber -> Rock -> Jet -> Rock
push c r j = let r' = S.map (^+^ j) r in if allows c r' then r' else r

fall :: Chamber -> Rock -> Maybe Rock
fall c r = let r' = S.map (^-^ unit _y) r in if allows c r' then Just r' else Nothing

dropRock :: State -> Rock -> State
dropRock s r =
    let r' = push (_chamber s) r (s ^. jets & head)
        h = maximum1Of (folded . _y) r'
     in case fall (_chamber s) r' of
            Nothing -> s & chamber %~ S.union r' & jets %~ tail & height %~ max h
            Just r'' -> dropRock (s & jets %~ tail) r''

dropFromTop :: State -> State
dropFromTop s =
    let r' = S.map (^+^ V2 2 (4 + _height s)) (s ^. rocks & head)
     in dropRock s r' & rocks %~ tail & units +~ 1

part1 :: [Jet] -> Int
part1 = succ . _height . until ((== 2022) . _units) dropFromTop . setup

profile :: State -> Set Pos
profile s =
    let peak = maximum1Of (folded . _y) raw
        raw = [V2 i (maximum1Of (folded . filteredBy (_x . only i) . _y) (_chamber s)) | i <- [0 .. 6]]
     in S.fromList $ fmap (^-^ V2 0 peak) raw

findCycle :: State -> (State, State)
findCycle s =
    let race f = tail . iterate f
        tortoise = race dropFromTop s
        hare = race (dropFromTop . dropFromTop) s
        different = (/=) `on` profile
        s' = fst . head $ dropWhile (uncurry different) $ zip tortoise hare
        s'' = head . dropWhile (different s') $ race dropFromTop s'
     in (s', s'')

part2 :: [Jet] -> Int
part2 js =
    let s = setup js
        (startS, repeatS) = findCycle s
        start = _units startS
        len = _units repeatS - start
        heightDiff = _height repeatS - _height startS
        afterStart = 1000000000000 - start
        (numCycles, dropsToDo) = divMod afterStart len
        finalHeight = succ . _height . (!! dropsToDo) $ iterate dropFromTop startS
     in finalHeight + (heightDiff * numCycles)

main :: IO ()
main = do
    input <- readJets <$> readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
