module Main where

  import PegTree
  import Prelude hiding (map) 
  import Data.Tree 
  
  type Ast = Tree String
  
  leaf :: String -> Ast
  leaf x = Node x []
  
  ifGrammar :: Grammar Ast
  ifGrammar = 
    [ ("E", eParser)
    , ("S", ifParser )
    ] 
  
  eParser :: PegParser Ast
  eParser = map leaf $ word "b"
  
  ifParser :: PegParser Ast
  ifParser = 
    ( word "if"   `seq'` \kwi   -> 
      eParser     `seq'` \cond  -> 
      word "then" `seq'` \kwt   -> 
      ifParser    `seq'` \bthen -> 
      word "else" `seq'` \kwe   ->
      ifParser    `seq'` \belse -> 
      ret $ Node "S" [leaf kwi, cond, leaf kwt, bthen, leaf kwe, belse])
    `alt` ( word "if"   `seq'` \kwi   -> 
            eParser     `seq'` \cond  -> 
            word "then" `seq'` \kwt   -> 
            ifParser    `seq'` \bthen -> 
            ret $ Node "S" [leaf kwi, cond, leaf kwt, bthen])
    `alt` (map leaf $ word "a")
  
  testParse :: Grammar Ast -> Nonterminal -> Input -> IO () 
  testParse grammar nonterminal input = do 
    putStrLn "\nParsing:"
    putStrLn input 
    putStrLn "Result:" 
    case parse grammar nonterminal input of 
      Success tree left -> do 
        putStrLn "Success"
        putStrLn $ "The rest of the input: " ++ show left 
        putStrLn $ drawTree tree 
      Failure -> 
        putStrLn "Failure"
  
  testIf :: Input -> IO () 
  testIf input = testParse ifGrammar "S" input 
  
  main :: IO ()
  main = do 
    testIf "a"
    testIf "if b then a" 
    testIf "if b then a else a" 
    testIf "if b then if b then a else a"
    testIf "if b then if b then a else a else if b then a else a"
  
    testIf "if b then" 
    testIf "if b then else a" 
    testIf "if a then a else if b then a else a"