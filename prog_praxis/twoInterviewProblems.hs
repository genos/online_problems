import Data.Set (difference, fromList, toList)

-- problem 1
search :: Int -> Maybe Int
search n = head' [k | k <- [100..999], n == (product . digits $ k)]
    where
        head' ks = if null ks then Nothing else Just $ head ks
        digits 0 = [] -- a lie, but OK here
        digits k = let (q, r) = k `divMod` 10 in r : digits q

-- problem 2
find2Extra :: [a] -> [a] -> (a, a)
find2Extra xs ys = (head d, head . tail $ d)
    where
        d = toList $ fromList ys `difference` fromList xs
