module C6ex where

import Data.Monoid
import qualified  Data.Foldable as F

data NonEmpty a = NonEmpty a [a]

instance F.Foldable NonEmpty where
  foldr f b (NonEmpty x [])     = f x b
  foldr f b (NonEmpty x (a:as)) = f x (F.foldr f b (NonEmpty a as)) 

  foldMap f (NonEmpty x (a:as)) = f x <> F.foldMap f (NonEmpty a as)
  foldMap f (NonEmpty x [])     = f x
--  foldMap f q m     = f x

--	foldl f b (NonEmpty x [])     = f b x
--  foldl f b (NonEmpty x (a:as)) = foldl f (f b x) $ NonEmpty a as 

