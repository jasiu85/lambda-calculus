module Lambda.Read() where

import Control.Applicative
import Data.Char(isAlpha)

import Lambda.Syntax
import Lambda.Manipulators(bind_params)

instance Read Term where
  readsPrec _ = runParser (bind_params <$> term)

data Parser a = Parser { runParser :: String -> [(a, String)] }

instance Functor Parser where
  fmap f pa = Parser $ \s -> [(b, s') | (a, s') <- runParser pa s, let b = f a]

instance Applicative Parser where
  pure a = Parser $ \s -> [(a, s)]
  pf <*> pa = Parser $ \s -> [(b, s'') | (f, s') <- runParser pf s, (a, s'') <- runParser pa s', let b = f a]

instance Alternative Parser where
  empty = Parser $ \s -> []
  pl <|> pr = Parser $ \s -> (runParser pl s) ++ (runParser pr s)

{-
instance Monad Parser where
  return a = Parser $ \s -> [(a, s)]
  pa >>= f = Parser $ \s -> [(b, s'') | (a, s') <- runParser pa s, let pb = f a, (b, s'') <- runParser pb s']
-}

char_satisfying p = Parser $ \s ->
  case s of
    [] -> []
    (c:cs) -> if p c
              then [(c,cs)]
              else []

char c = char_satisfying (c ==)

var = to_string <$> char_satisfying isAlpha where
  to_string c = [c]

with_parens p = char '(' *> p <* char ')'

at_least 0 p = many p
at_least 1 p = some p
at_least n p = (:) <$> p <*> (at_least (n - 1) p) 

at_least_separated_by n px ps = (:) <$> px <*> (at_least (n-1)  $ ps *> px)

separated_by = at_least_separated_by 1

term = term_var <|> term_lambda <|> term_apply <|> (with_parens term)

term_var = (TermVar . VarFree) <$> var

term_lambda = (flip $ foldr TermLambda) <$> ((char '\\') *> (var `separated_by` (char ' ')) <* (char '.')) <*> term

term_apply = with_parens term_apply' where
  term_apply' = (foldl1 TermApply) <$> (at_least_separated_by 2 term (char ' '))
