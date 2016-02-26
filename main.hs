import Control.Applicative

import Lambda.Syntax
import Lambda.Manipulators
import Lambda.Show
import Lambda.Read
import Lambda.Eval

main = do
  putStrLn "Enter term:"
  term_str <- getLine
  let term = read term_str
  putStrLn ("Named:       " ++ (show $ NamedTerm term))
  putStrLn ("de Bruijn:   " ++ (show $ DeBruijnTerm term))
  putStrLn ("AST:         " ++ (show $ AstTerm term))
  putStrLn ("CallByName:  " ++ (show $ NamedTerm $ evalCallByName term))
  putStrLn ("CallByValue: " ++ (show $ NamedTerm $ evalCallByValue term))
