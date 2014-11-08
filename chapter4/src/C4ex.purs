module C4ex where

import Data.Array (null, filter, (..), concat)
import Data.Array.Unsafe (tail, head)
import Control.MonadPlus
import Data.Foldable (product, any)
import Debug.Trace

length2 :: forall a. [a] -> Number
length2 arr = 
  if null arr
  then 0 
  else 1 + length2 (tail arr)

-- excercise asks for recursive impl but I did this...
evenLength :: forall a. [a] -> Boolean
evenLength arr = (length2 arr) % 2 == 0
   
evenLength' :: forall a. [a] -> Boolean
evenLength' []  = true
evenLength' arr = not $ evenLength' $ tail arr

numEvens :: [Number] -> Number
numEvens [] = 0
numEvens (x:xs) = (if x % 2 == 0 then 1 else 0) + numEvens xs
  
squares :: [Number] -> [Number]
squares xs = (\n -> n*n) <$> xs

noNegs :: [Number] -> [Number]
noNegs xs = filter (\x -> x > 0) xs

(<#>) = filter
infix 5 <#>

factors :: Number -> [[Number]]
factors n = filter (\xs -> product xs == n) $ do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  return [i,j] 


isPrime :: Number -> Boolean
isPrime n = (Data.Array.length (factors n)) == 1

cartProd :: [Number] -> [Number] -> [[Number]]
cartProd a b = do
  i <- a
  j <- b
  return [i,j]

pythTriple :: Number -> [[Number]]
pythTriple n = do
  a <- 1..n
  b <- 1..n
  c <- 1..n
  guard $ a*a + b*b == c*c
  return [a,b,c]

count :: forall a. (a -> Boolean) -> [a] -> Number
count _ [] = 0
count p xs = count' 0 p xs
  where
  count' acc _ [] = acc
  count' acc p (x:xs) = 
     let i = if p x then 1 else 0
     in count' (acc+i) p xs
    
-- Not tail recursive in purescript
count2 :: forall a. (a -> Boolean) -> [a] -> Number
count2 _ [] = 0
count2 p (x : xs) = (addOneIfTrue p x ) + count p xs 
  where
  addOneIfTrue p x = if p x then 1 else 0


reverse :: forall a. [a] -> [a] 
reverse = reverse' []
  where
  reverse' acc [] = acc
  reverse' acc (x : xs) = reverse' (x : acc) xs

--main = trace "hello world"

-- does not work
--main = do
--   ns <- [12, 13, 14,1,2] 
--   a  <- count (\x -> x > 10) ns
--   b  <- count2 (\x -> x > 10) ns
--   c  <- concat a b
--   print (head c)
