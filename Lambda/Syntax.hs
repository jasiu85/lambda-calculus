module Lambda.Syntax (VarName(..), Term(..)) where

data VarName = VarFree String
             | VarBound Int
  deriving (Eq)

data Term = TermVar VarName
          | TermConst Int
          | TermLambda String Term
          | TermAdd Term Term
          | TermMul Term Term
          | TermApply Term Term
  deriving (Eq)
