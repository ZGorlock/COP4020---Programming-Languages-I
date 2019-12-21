module ExpressionsParser where

import Parser
import Expressions
import Data.Char

parser :: Parse Char Expr
parser s = expressionParser(preprocess(s))

expressionParser :: Parse Char Expr
expressionParser = litParse `alt` varParse `alt` opExpParse

preprocess :: String -> String
preprocess []     = []
preprocess (x:xs)
    | isSpace x = preprocess xs
    | otherwise = [x] ++ preprocess xs

opExpParse :: Parse Char Expr
opExpParse
  = (
      token  '('  >*>
      parser      >*>
      spot   isOp >*>
      parser      >*>
      token  ')'
    ) `build` makeExpr

makeExpr :: (Char, (Expr, (Char, (Expr, Char)))) -> Expr
makeExpr (_,(e1,(bop,(e2,_)))) = Op (charToOp bop) e1 e2

litParse :: Parse Char Expr
litParse 
  = (
      (optional (token '-')) >*>
      (neList (spot Parser.isDigit))
    ) `build` (charlistToExpr . uncurry (++))

varParse :: Parse Char Expr
varParse = spot isVar `build` Var

isVar :: Char -> Bool
isVar x = ('a' <= x && x <= 'z')

charToOp :: Char -> Ops
charToOp '+' = Add
charToOp '-' = Sub
charToOp '*' = Mul
charToOp '/' = Div

isOp :: Char -> Bool
isOp '+' = True
isOp '-' = True
isOp '*' = True
isOp '/' = True
isOp _   = False

charlistToExpr :: [Char] -> Expr
charlistToExpr str = Lit (read str)

