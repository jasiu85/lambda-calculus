import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer

import Lambda.Syntax
import Lambda.Manipulators
import Lambda.Show
import Lambda.Read
import Lambda.Eval

main = do
  putStrLn "Enter term:"
  term_str <- getLine
  let inputTerm = read term_str
  putStrLn (show $ NamedTerm inputTerm)
  let resultTerm = evalCallByName inputTerm
  putStrLn (show $ NamedTerm resultTerm)
