module C6ex where

import Data.Array

newtype Complex = Complex 
   { real :: Number
   , imaginary :: Number 
   }


instance complexShow :: Show Complex where
  show (Complex c) = "r =  " ++ (show c.real) ++ "i = " ++ (show c.imaginary)
--  show (Complex { real = r, imaginary = c }) = "r =  " ++ (show c) ++ "i = " ++ (show c)


data NonEmpty a = NonEmpty a [a]

instance neSG :: Semigroup (NonEmpty a) where
  (<>) (NonEmpty a as) (NonEmpty b bs) = NonEmpty a ([b] <> as <> bs)

-- WTF. why doesn't this work? 
instance complexSemiGroup :: Semigroup Complex where
  (<>) c c'= let i = c.imaginary in Complex { real: 1, imaginary: i } 
 -- (<>) c c'= Complex (c.real + c'.real) (c.imaginary + c'.imaginary)
 -- (<>) c c'= Complex { real: 1, imaginary: (c.imaginary + c'.imaginary) } 
