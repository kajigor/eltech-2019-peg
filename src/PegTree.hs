module PegTree where

  import Prelude hiding (seq, map)
  
  data Result a t = Success t a | Failure deriving Show
  type Input = String 
  type PegParser t = Input -> Result Input t
  type Nonterminal = String 
  type Grammar t = [(Nonterminal, PegParser t)]
  
  eps :: PegParser String 
  eps input = Success "" input
  
  term :: Char -> PegParser Char 
  term c [] = Failure 
  term c (x:xs) | c == x = Success c xs 
  term _ xs = Failure
  
  nonterm :: Nonterminal -> Grammar t -> PegParser t 
  nonterm a g = go a g where 
    go a [] = error $ "There is no nonterminal " ++ show a ++ " in the grammar" 
    go a ((n, p):xs) | a == n = p 
    go a (_:xs) = go a xs 
  
  seq :: PegParser t -> (t -> PegParser u) -> PegParser u 
  seq p q = \input -> 
    case p input of 
      Success t input' -> q t input'
      Failure -> Failure 
  
  seq' :: PegParser t -> (t -> PegParser u) -> PegParser u
  seq' p q = \input -> 
    case p input of 
      Success t input' -> q t (dropWhitespaces input')
      Failure -> Failure 
      where 
        dropWhitespaces (' ':xs) = dropWhitespaces xs 
        dropWhitespaces x = x 
  
  alt :: PegParser t -> PegParser t -> PegParser t 
  alt p q = \input -> 
    case p input of 
      Failure -> q input 
      x -> x 
  
  star :: Monoid t => PegParser t -> PegParser t 
  star p = \input -> 
    case p input of 
      Success t input' -> map (t <>) (star p) input'
      Failure -> Success mempty input
    
  not :: Monoid t => PegParser t -> PegParser t 
  not p = \input -> 
    case p input of 
      Failure -> Success mempty input 
      _ -> Failure
  
  word :: String -> PegParser String
  word [] = eps 
  word (x:xs) = term x `seq` (\c -> map (c:) $ word xs) 
  
  map :: (a -> b) -> PegParser a -> PegParser b 
  map f p = \input -> 
    case p input of 
      Success t input' -> Success (f t) input'
      Failure -> Failure
  
  ret :: a -> PegParser a 
  ret x = \input -> Success x input
  
  parse :: Grammar t -> Nonterminal -> Input -> Result Input t
  parse grammar nonterminal input = 
    nonterm nonterminal grammar input 