import Lambda.Syntax
import Lambda.Analysis
import Lambda.Named
import Lambda.Show

test_term =
  (TermApply
    (TermLambda (TermLambda
      (TermApply
        (TermApply (TermVar 2) (TermVar 1))
        (TermApply (TermVar 3) (TermVar 0)))))
    (TermVar 3))

fv_names = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"]
bv_names = ["x", "y", "z", "t", "u", "v", "w"]
named_term = to_named fv_names bv_names test_term
unnamed_term = from_named fv_names named_term

main = do
  putStrLn ("Term:      " ++ (show test_term))
  putStrLn ("Named:     " ++ (show named_term))
  putStrLn ("Unnamed:   " ++ (show unnamed_term))
  putStrLn ("Rank:      " ++ (show (rank test_term)))
  putStrLn ("Free vars: " ++ (show (free_vars test_term)))
