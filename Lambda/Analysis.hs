module Lambda.Analysis(rank, free_vars) where

import Data.List(sort, group)
import Lambda.Syntax

rank (TermVar k) = k + 1
rank (TermLambda t) = (rank t) - 1
rank (TermApply tf ta) = maximum [(rank tf), (rank ta)]

free_vars t =
  ((map head) . group . sort) (free_vars' 0 t)
  where
    free_vars' lambda_nest_level (TermVar k) =
      if k < lambda_nest_level
      then []
      else [k - lambda_nest_level]
    free_vars' lambda_nest_level (TermLambda t) =
      free_vars' (lambda_nest_level + 1) t
    free_vars' lambda_nest_level (TermApply tf ta) = fvf ++ fva
      where
        fvf = free_vars' lambda_nest_level tf
        fva = free_vars' lambda_nest_level ta
        
