module Main where

import Peg
import Prelude hiding (seq)

grammarExpression :: Grammar 
grammarExpression = undefined

testExpression :: Input -> IO () 
testExpression input = 
  testParse grammarExpression "E" input 

testParse :: Grammar -> Nonterminal -> Input -> IO ()
testParse grammar nonterminal input = do 
  putStrLn "\nParsing:"
  putStrLn input 
  putStrLn "Result:" 
  putStrLn $ show $ parse grammar nonterminal input

-- N -> a N / a 
parserN :: PegParser
parserN = 
  (term 'a' `seq` parserN) 
  `alt` (term 'a')

grammarN :: Grammar
grammarN = [("N", parserN)] 

testN :: Input -> IO () 
testN input = testParse grammarN "N" input 

-- N' -> a / a N'
parserN' :: PegParser 
parserN' = 
  term 'a'
  `alt` (term 'a' `seq` parserN')

grammarN' :: Grammar
grammarN' = [("N", parserN')]

testN' :: Input -> IO () 
testN' input = testParse grammarN' "N" input 

ifGrammar :: Grammar 
ifGrammar = 
  [ ("E", eParser)
  , ("S", ifParser )
  ] 

-- E -> 'b' 
-- S -> 'if' E 'then' S 'else' S 
--    / 'if' E 'then' S 
--    / 'a'

eParser :: PegParser 
eParser = term 'b'

ifParser :: PegParser 
ifParser = 
  (word "if" `seq''` eParser `seq''` word "then" `seq''` ifParser `seq''` word "else" `seq''` ifParser )
  `alt` (word "if" `seq''` eParser `seq''` word "then" `seq''` ifParser)
  `alt` (term 'a')

testIf :: Input -> IO () 
testIf input = testParse ifGrammar "S" input 

main :: IO ()
main = do
  putStrLn "\nN -> a N / a\n"
  testN "aaa"
  testN "abc" 
  testN "cba"

  putStrLn "\nN -> a / a N\n"
  testN' "aaa"
  testN' "abc" 
  testN' "cba"

  putStrLn "\nE -> 'b'\nS -> 'if' E 'then' S 'else' S\n   / 'if' E 'then' S\n   / 'a'\n" 
  testIf "if b then a" 
  testIf "if b then a else a" 
  testIf "if b then a else if b then a else a"

  testIf "if b then" 
  testIf "if b then else a" 
  testIf "if a then a else if b then a else a"

  -- testExpression "123"
  -- testExpression "12 + 3 * 4"

  -- testExpression "12 / a"
  -- testExpression "abc"

