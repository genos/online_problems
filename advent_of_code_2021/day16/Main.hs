module Main where

import Data.Bifunctor     (first)
import Data.Bits          (testBit, unsafeShiftL)
import Data.Bool          (bool)
import Data.Char          (digitToInt)
import Data.Foldable      (foldl', traverse_)
import Data.Function      (on)
import Data.List.NonEmpty (NonEmpty (..), fromList)

readBits :: String -> [Bool]
readBits = concatMap (fourBits . digitToInt)
  where fourBits n = [ n `testBit` i | i <- [3, 2, 1, 0] ]

bToI :: [Bool] -> Int
bToI = foldl' (\i b -> (i `unsafeShiftL` 1) + bool 0 1 b) 0

type Parser a = [Bool] -> (a, [Bool])

int :: Int -> Parser Int
int n = first bToI . splitAt n

data Operation = OSum | OMul | OMin | OMax deriving Enum
data Comparator = CGT | CLT | CEQ deriving Enum

data Packet = Literal !Int !Int
            | Operation !Int !Operation !(NonEmpty Packet)
            | Comparison !Int !Comparator !Packet !Packet

literal :: Int -> Parser Packet
literal v = first (Literal v . bToI) . go
 where
  go [] = error "Empty list passed to `literal`"
  go bs =
    let (x : ys, bs') = splitAt 5 bs
    in  if not x then (ys, bs') else first (ys ++) $ go bs'

subN :: Int -> Parser [Packet]
subN n = go []
 where
  go ps bs =
    if length ps >= n then (ps, bs) else let (p, bs') = parse bs in go (p : ps) bs'

subT :: Int -> Parser [Packet]
subT n = go 0 []
 where
  go i ps bs = if i >= n
    then (ps, bs)
    else
      let (p, bs') = parse bs
          d        = ((-) `on` length) bs bs'
      in  go (i + d) (p : ps) bs'

dispatch :: Parser [Packet]
dispatch []       = error "Empty list passed to `dispatch`"
dispatch (b : bs) = if b
  then let (n, bs') = (first bToI $ splitAt 11 bs) in subN n bs'
  else let (n, bs') = (first bToI $ splitAt 15 bs) in subT n bs'

operation :: Int -> Int -> Parser Packet
operation v o = first (Operation v (toEnum o) . fromList) . dispatch

comparison :: Int -> Int -> Parser Packet
comparison v c bits = (Comparison v (toEnum c) x y, bits')
 where
  (ps, bits') = dispatch bits
  [x , y    ] = reverse $ take 2 ps

parse :: Parser Packet
parse bs = p
 where
  (v, bs' ) = int 3 bs
  (t, bs'') = int 3 bs'
  p         = case t of
    n | n `elem` [0 .. 3] -> operation v n bs''
    4                     -> literal v bs''
    n | n `elem` [5 .. 7] -> comparison v (n - 5) bs''
    n                     -> error $ "Unexpected integer in `parse`: " ++ show n

sumVersions :: Packet -> Int
sumVersions (Literal v _       ) = v
sumVersions (Operation v _ ps  ) = v + sum (sumVersions <$> ps)
sumVersions (Comparison v _ x y) = v + sumVersions x + sumVersions y

eval :: Packet -> Int
eval (Literal _ v     ) = v
eval (Operation _ o ps) = op $ eval <$> ps
  where op = [sum, product, minimum, maximum] !! fromEnum o
eval (Comparison _ c x y) = bool 0 1 $ cmp (eval x) (eval y)
  where cmp = [(>), (<), (==)] !! fromEnum c

solve :: (Packet -> Int) -> String -> Int
solve = (. fst . parse . readBits)

part1 :: String -> Int
part1 = solve sumVersions

part2 :: String -> Int
part2 = solve eval

main :: IO ()
main = do
  input <- readFile "input.txt"
  traverse_ (print . ($ input)) [part1, part2]
