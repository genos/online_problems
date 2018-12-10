module Circle where

import           Data.Sequence (Seq (..), (<|), (|>))
import qualified Data.Sequence as S

-- based on `PointedList`'s `Data.List.PointedList.Circular`
data Circle a = C { _prefix :: Seq a
                  , _focus  :: a
                  , _suffix :: Seq a
                  }

instance (Show a) => Show (Circle a) where
  show (C p f s) = show p <> " " <> show f <> " " <> show s

instance Functor Circle where
  fmap g (C p f s) = C (g <$> p) (g f) (g <$> s)

instance Foldable Circle where
  foldMap g (C p f s) = foldMap g p <> g f <> foldMap g s

singleton :: a -> Circle a
singleton a = C S.empty a S.empty

next :: Circle a -> Circle a
next c@(C p f s) | S.null p && S.null s = c
                 | S.null s = let (f' :<| s') = p in C S.empty f' (s' |> f)
                 | otherwise = let (f' :<| s') = s in C (p |> f) f' s'

prev :: Circle a -> Circle a
prev c@(C p f s) | S.null p && S.null s = c
                 | S.null p = let (p' :|> f') = s in C (p' |> f) f' S.empty
                 | otherwise = let (p' :|> f') = p in C p' f' (f <| s)

moveRight :: Int -> Circle a -> Circle a
moveRight 0 c = c
moveRight n c | n < 0     = moveLeft (-n) c
              | otherwise = moveRight (n - 1) $ next c

moveLeft :: Int -> Circle a -> Circle a
moveLeft 0 c = c
moveLeft n c | n < 0     = moveRight (-n) c
             | otherwise = moveLeft (n - 1) $ prev c

insertLeft :: a -> Circle a -> Circle a
insertLeft x (C p f s) = C p x (f <| s)

insertRight :: a -> Circle a -> Circle a
insertRight x (C p f s) = C (p |> f) x s

deleteLeft :: Circle a -> Circle a
deleteLeft (C p _f s) | S.null p && S.null s = error "deleteLeft on singleton"
                      | S.null s = let (f' :<| s') = p in C S.empty f' s'
                      | otherwise = let (f' :<| s') = s in C p f' s'

deleteRight :: Circle a -> Circle a
deleteRight (C p _f s) | S.null p && S.null s = error "deleteRight on singleton"
                       | S.null p = let (p' :|> f') = s in C p' f' S.empty
                       | otherwise = let (p' :|> f') = p in C p' f' s
