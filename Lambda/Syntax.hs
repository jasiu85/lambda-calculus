module Lambda.Syntax (VarName(..), Term(..)) where

data VarName = VarFree String
             | VarBound Int
  deriving (Eq)

data Term = TermVar VarName
          | TermConst Integer
          | TermLambda String Term
          | TermAdd Term Term
          | TermMul Term Term
          | TermPow Term Term
          | TermApply Term Term
  deriving (Eq)
