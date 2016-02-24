import Lambda.Syntax
import Lambda.Manipulators
import Lambda.Show
import Lambda.Read

main = do
  putStrLn "Enter term:"
  term_str <- getLine
  let term' = read term_str
  let term = reduce term'
  putStrLn ("Named    : " ++ (show $ NamedTerm term))
  putStrLn ("de Bruijn: " ++ (show $ DeBruijnTerm term))
{-
  putStrLn ("Unnamed:   " ++ (show unnamed_term))
  putStrLn ("Renamed:   " ++ (show renamed_term))
  putStrLn ("Rank:      " ++ (show (rank unnamed_term)))
  putStrLn ("Free vars: " ++ (show (free_vars unnamed_term)))
-}
