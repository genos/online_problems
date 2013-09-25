import Data.List (isInfixOf)

powersOf2 :: [Integer]
powersOf2 = 1 : map (2*) powersOf2

findPow2 :: String -> Maybe Int
findPow2 s = aux 0
    where
        aux i | i > 1000                            = Nothing
              | isInfixOf s $ show (powersOf2 !! i) = Just i
              | otherwise                           = aux $ i + 1
