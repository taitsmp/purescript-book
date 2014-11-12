module C6ex where

import Data.Array

-- newtype constructors must take exactly one argument.  This actually works. 
newtype Complex = Complex
   { real :: Number
   , imaginary :: Number 
   }

-- experimenting with records and types synonyms
type Cplx = { real :: Number, imaginary :: Number }

instance complexShow :: Show Complex where
  show (Complex c) = "r =  " ++ (show c.real) ++ "i = " ++ (show c.imaginary)
--  show (Complex { real = r, imaginary = c }) = "r =  " ++ (show c) ++ "i = " ++ (show c)

data NonEmpty a = NonEmpty a [a]

instance neSG :: Semigroup (NonEmpty a) where
  (<>) (NonEmpty a as) (NonEmpty b bs) = NonEmpty a ([b] <> as <> bs)

instance complexSemiGroup :: Semigroup Complex where
   (<>) (Complex c) (Complex c') = Complex { real: c.real + c'.real, imaginary: c.imaginary + c'.imaginary }

-- type synonym instances are not allowed. 
--instance complexSemiGroup :: Semigroup Cplx where
--   (<>) c c' = { real: c.real + c'.real, imaginary: c.imaginary + c'.imaginary }

instance nonEmptyFoldable :: Foldable (NonEmpty a) where
  foldr f b (NonEmpty x []) = f x b
  foldr f b (NonEmpty x (a:as)) = f x (foldr f b (NonEmpty a as)) 

	foldl f b (NonEmpty x [])     = f b x
  foldl f b (NonEmpty x (a:as)) = foldl f (f b x) $ NonEmpty a as 

	foldMap f (NonEmpty x []) m = (f x) <> m
	foldMap f (NonEmpty x (a:as)) m = (f x) <> (foldMap f (NonEmpty a as))
