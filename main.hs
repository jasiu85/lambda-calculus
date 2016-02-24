import Lambda.Syntax
import Lambda.Analysis
import Lambda.Show
import Lambda.Read

main = do
  putStrLn "Enter term:"
  term_str <- getLine
  let term = read term_str
  putStrLn ("Original:  " ++ (show term))
  putStrLn ("de Bruijn: " ++ (show $ DeBruijn term))
{-
  putStrLn ("Unnamed:   " ++ (show unnamed_term))
  putStrLn ("Renamed:   " ++ (show renamed_term))
  putStrLn ("Rank:      " ++ (show (rank unnamed_term)))
  putStrLn ("Free vars: " ++ (show (free_vars unnamed_term)))
-}
