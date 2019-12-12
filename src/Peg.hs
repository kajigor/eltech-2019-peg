module Peg where

import Prelude hiding (seq)

type Input = String 

type PegParser = Input -> Result 

data Result = Failure 
            | Success Input 
            deriving Show 

type Nonterminal = String 

type Grammar = [ (Nonterminal, PegParser) ]

star :: PegParser -> PegParser 
star p = \input -> 
  undefined 

not :: PegParser -> PegParser
not p = \input -> 
  undefined

eps :: PegParser 
eps input = Success input 

term :: Char -> PegParser 
term c (x:xs) | x == c = Success xs 
term _ _ = Failure 

seq :: PegParser -> PegParser -> PegParser 
seq p q = \input -> 
  case p input of 
    Failure -> Failure 
    Success input' -> q input' 

seq'' :: PegParser -> PegParser -> PegParser 
seq'' p q = \input -> 
  case p input of 
    Failure -> Failure 
    Success xs -> q (dropWhitespace xs) 
      where 
        dropWhitespace (' ' :xs) = dropWhitespace xs 
        dropWhitespace xs = xs  

word :: String -> PegParser 
word [] = eps 
word (c:xs) = term c `seq` word xs 
    
nonterm :: Nonterminal -> Grammar -> PegParser 
nonterm n g = go n g where 
  go n [] = error ("There is no nonterminal " ++ n)
  go n ((x, p) : g) | x == n = p 
  go n (_ : g) = go n g 

alt :: PegParser -> PegParser -> PegParser 
alt p q = \input -> 
  case p input of 
    Success x -> Success x 
    Failure -> q input 

parse :: Grammar -> Nonterminal -> Input -> Result 
parse grammar nonterminal input = 
  (nonterm nonterminal grammar) input 

parse' :: PegParser -> Input -> Result 
parse' parser input = parser input 


 