module Lambda.Printing(pretty_print) where

import Lambda.Syntax

pretty_print (TermVar k) = show k
pretty_print (TermLambda t) = "\\." ++ (pretty_print t)
pretty_print (TermApply tf ta) = "(" ++ (pretty_print tf) ++ " " ++ (pretty_print ta) ++ ")"
