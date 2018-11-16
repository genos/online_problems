module Main where

import Control.Arrow ((&&&))
import Data.Foldable (traverse_)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Data.Set as S

pairs :: M.Map Char Char
pairs = M.fromList [(')', '('), (']', '['), ('}', '{')]

open :: S.Set Char
open = S.fromList . M.elems $ pairs

shut :: S.Set Char
shut = M.keysSet pairs

ok :: String -> Bool
ok = f []
    where
        f s []     = null s
        f s (c:cs) | S.member c open          = f (c:s) cs
                   | S.member c shut && g s c = f (tail s) cs
                   | otherwise                = f s cs
        g s c      = not (null s) && (head s == fromJust (M.lookup c pairs))

main :: IO ()
main = traverse_ (print . (id &&& ok)) . words $
    "{} [] () a(b)c abc[d] a(b)c{d[e]} {] (] a(b]c abc[d} a(b)c{d[e}]"
