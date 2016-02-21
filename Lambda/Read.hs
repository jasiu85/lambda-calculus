module Lambda.Read() where

import Control.Applicative
import Data.Char(isAlpha)

import Lambda.Named

instance Read NamedTerm where
  readsPrec _ = runParser term

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

var = (:[]) <$> char_satisfying isAlpha

with_parens p = char '(' *> p <* char ')'

some_separated_by px ps = (:) <$> px <*> (many $ ps *> px)

term = term_var <|> term_lambda <|> term_apply <|> (with_parens term)

term_var = NamedTermVar <$> var

term_lambda = (flip $ foldr NamedTermLambda) <$> ((char '\\') *> (var `some_separated_by` (char ' ')) <* (char '.')) <*> term

term_apply =
  with_parens term_apply'
  where
    term_apply' = (foldl1 NamedTermApply) <$> (term `some_separated_by` (char ' '))
