import Lambda.Syntax
import Lambda.Printing
import Lambda.Analysis
import Lambda.Named

test_term = TermApply (TermLambda (TermLambda (TermVar 2))) (TermVar 3)

fv_names = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"]
bv_names = ["x", "y", "z", "t", "u", "v", "w"]
named_term = to_named fv_names bv_names test_term
unnamed_term = from_named fv_names named_term

main = do
  putStrLn ("Term:      " ++ (show test_term))
  putStrLn ("Named:     " ++ (show named_term))
  putStrLn ("Unnamed:   " ++ (show unnamed_term))
  putStrLn ("Term:      " ++ (pretty_print test_term))
  putStrLn ("Rank:      " ++ (show (rank test_term)))
  putStrLn ("Free vars: " ++ (show (free_vars test_term)))
