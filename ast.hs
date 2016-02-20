data Term = TermVar Int
          | TermApply Term Term
          | TermLambda Term
