module Lambda.Syntax (Term(..)) where

data Term = TermVar Int
          | TermLambda Term
          | TermApply Term Term
    deriving (Eq, Show)
