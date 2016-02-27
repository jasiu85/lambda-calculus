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
  let term = read term_str
  putStrLn (show $ NamedTerm term)
  let reductions = snd $ runIdentity $ runWriterT $ (`runReaderT` id) $ (evalCallByValue term)
  mapM_ (putStrLn . show .NamedTerm) reductions
--  putStrLn ("CallByValue: " ++ (show $ NamedTerm $ evalCallByValue term))
