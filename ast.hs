module LambdaCalculus.AST (Term) where

data Term = TermVar Int
          | TermApply Term Term
          | TermLambda Term
