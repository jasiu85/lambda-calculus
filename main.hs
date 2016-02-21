import Lambda.Syntax
import Lambda.Analysis
import Lambda.Named
import Lambda.Show
import Lambda.Read

test_term :: NamedTerm
test_term = read "\\x.x"

fv_names = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"]
bv_names = ["x", "y", "z", "t", "u", "v", "w"]
named_term = test_term
unnamed_term = from_named fv_names named_term

main = do
  putStrLn "Enter term:"
  str_term <- getLine
  let named_term = read str_term
  let unnamed_term = from_named fv_names named_term
  let renamed_term = to_named fv_names bv_names unnamed_term
  putStrLn ("Original:  " ++ str_term)
  putStrLn ("Named:     " ++ (show named_term))
  putStrLn ("Unnamed:   " ++ (show unnamed_term))
  putStrLn ("Renamed:   " ++ (show renamed_term))
  putStrLn ("Rank:      " ++ (show (rank unnamed_term)))
  putStrLn ("Free vars: " ++ (show (free_vars unnamed_term)))
