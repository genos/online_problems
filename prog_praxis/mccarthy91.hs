import Control.Monad.Writer

-- Simple recursive version
m :: Integer -> Integer
m n | n > 100   = n - 10
    | otherwise = m . m $ n + 11

-- Recursive with counter and writer for output
m' :: Integer -> Int -> Writer [String] Integer
m' n c | c <= 0    = do
            tell [show n]
            return n
       | otherwise = do
            let crc = concat . replicate c
            tell [crc "M(" ++ show n ++ crc ")"]
            if n > 100 then
                m' (n - 10) (c - 1)
            else
                m' (n + 11) (c + 1)

-- Example
main :: IO ()
main = mapM_ putStrLn $ snd $ runWriter (m' 87 1)
