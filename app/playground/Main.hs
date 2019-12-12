module Main where 

import Debug.Trace (trace)

f x = x 

g :: Int -> Int 
g y = y + 1 

fib :: Int -> Int 
fib 0 = 0 
fib 1 = 1 
fib x = fib (x - 1) + fib (x - 2)

fib' :: Double -> Double
fib' x = 
  if x == 0 
  then 0 
  else if x == 1 
       then 1  
       else  fib' (x - 1) + fib' (x - 2)

map' :: (a -> b) -> [a] -> [b]
map' f [] = [] 
map' f (h:t) = f h : map' f t 

data ListInt = Cons Int ListInt
             | Nil  
             deriving Show 

isOdd :: Int -> Bool 
isOdd 0 = False 
isOdd 1 = True 
isOdd x = isOdd (x - 2)

isOdd' :: Int -> Bool 
isOdd' x = x `mod` 2 == 1 

-- mod :: Int -> Int -> Int 

-- data Bool = True 
--           | False 

data Oddity = Even 
            | Odd 

determineOddity :: Int -> Oddity 
determineOddity x | x `mod` 2 == 0 = Even 
determineOddity x = Odd 

-- dO x = 
--   if x `mod` 2 == 0 
--   then Even 
--   else Odd 

data Tree a = Node a (Tree a) (Tree a) 
            | Leaf a 
            deriving Show 

tree0 = Leaf 13 
tree1 = Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5)

mapTree :: (a -> b) -> Tree a -> Tree b 
mapTree f (Leaf x) = Leaf (f x) 
mapTree f (Node x l r) = 
  Node (f x) (mapTree f l) (mapTree f r) 

main :: IO () 
main = do
  putStrLn "Hello haskell!"
  
  print (f 13)
  print (g 42)
  print (fib 10)
  print (fib' 10)

  print (isOdd 13)
  print (isOdd' 13)

  let x = 1 : 2 : x
  -- print (map' g x) 
  
  print Nil  -- [] 
  print (Cons 13 Nil) -- (13 : []) == [13]
  print (Cons 1 (Cons 2 (Cons 3 Nil)))  -- (1 : (2 : (3 : []))) == [1, 2, 3] 

  print tree0 
  print $ mapTree g tree0 

  print tree1 
  print $ mapTree (\x -> x * 3) tree1 


