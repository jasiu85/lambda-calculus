module Lambda.Syntax (VarName(..), Term(..)) where

data VarName = VarFree String
             | VarBound Int
  deriving (Eq)

data Term = TermVar VarName
          | TermLambda String Term
          | TermApply Term Term
  deriving (Eq)
