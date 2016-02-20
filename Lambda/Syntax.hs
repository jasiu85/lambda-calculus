module Lambda.Syntax (Term(..)) where

data Term = TermVar Int
          | TermApply Term Term
          | TermLambda Term
