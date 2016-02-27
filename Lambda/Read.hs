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

separatedByLeft elemParser sepParser =
  combineAll <$> elemParser <*> (many $ (,) <$> sepParser <*> elemParser) where
    combineAll elem rest = foldl combineTwo elem rest
    combineTwo elemL (sepFun, elemR) = sepFun elemL elemR

separatedByRight elemParser sepParser =
  combineAll <$> elemParser <*> (many $ (,) <$> sepParser <*> elemParser) where
    combineAll elem rest = (foldr combineTwo id rest) elem
    combineTwo (sepFun, elemL) funR = \elem -> sepFun elem (funR elemL)

term =
  (termFun $ termApply $ termHighestPrec) <|>
  (termApply $ termHighestPrec) <|>
  (termHighestPrec)

termFun higherPrecParser =
  ((char '\\') *> argList <* (char '.')) <*> higherPrecParser where
    argList = (buildFun <$> var) `separatedByRight` (const buildNestedFuns <$> char ' ')
    buildFun var = \body -> TermLambda var body
    buildNestedFuns funL funR = \body -> funL (funR body)

termApply higherPrecParser =
  higherPrecParser `separatedByLeft` ((const buildApply) <$> char ' ') where
    buildApply = TermApply

termHighestPrec = termVar <|> termParens

termVar = (TermVar . VarFree) <$> var

termParens = with_parens term
