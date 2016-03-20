import Control.Monad.RWS

import Lambda.Syntax
import Lambda.Manipulators
import Lambda.Show
import Lambda.Read
import Lambda.Eval

main = do
  putStrLn "Enter term:"
  term_str <- getLine
  let inputTerm = read term_str
  putStrLn "Call-by-value:"
  putStrLn (show $ NamedTerm inputTerm)
  let (_, stepTerms) = evalRWS (evalCallByValue inputTerm) (TermLens id) inputTerm
  mapM_ (putStrLn .show . NamedTerm) stepTerms
  putStrLn "Call-by-name:"
  putStrLn (show $ NamedTerm inputTerm)
  let (_, stepTerms) = evalRWS (evalCallByName inputTerm) (TermLens id) inputTerm
  mapM_ (putStrLn .show . NamedTerm) stepTerms
