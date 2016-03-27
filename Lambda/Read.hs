module Lambda.Read() where

import Control.Applicative
import Data.Char(isAlpha, isDigit, ord)

import Lambda.Syntax
import Lambda.Manipulators

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

constInt = to_int <$> char_satisfying isDigit where
  to_int c = (ord c) - (ord '0')

with_parens p = char '(' *> p <* char ')'

separatedByLeft elemParser sepParser =
  combineAll <$> elemParser <*> (many $ (,) <$> sepParser <*> elemParser) where
    combineAll elem rest = foldl combineTwo elem rest
    combineTwo elemL (sepFun, elemR) = sepFun elemL elemR

separatedByRight elemParser sepParser =
  combineAll <$> elemParser <*> (many $ (,) <$> sepParser <*> elemParser) where
    combineAll elem rest = (foldr combineTwo id rest) elem
    combineTwo (sepFun, elemL) funR = \elem -> sepFun elem (funR elemL)

term = termFun $ termAdd $ termMul $ termApply $ termHighestPrec

termFun higherPrecParser = higherPrecParser <|> funParser where
  funParser = ((char '\\') *> argList <* (char '.')) <*> higherPrecParser where
    argList = (buildFun <$> var) `separatedByRight` (const buildNestedFuns <$> char ' ')
    buildFun var = \body -> TermLambda var body
    buildNestedFuns funL funR = \body -> funL (funR body)

termAdd higherPrecParser =
  higherPrecParser `separatedByLeft` opParser where
    opParser = (const TermAdd) <$> (char ' ' *> char '+' <* char ' ')

termMul higherPrecParser =
  higherPrecParser `separatedByLeft` opParser where
    opParser = (const TermMul) <$> (char ' ' *> char '*' <* char ' ')

termApply higherPrecParser =
  higherPrecParser `separatedByLeft` opParser where
    opParser = (const TermApply) <$> char ' '

termHighestPrec = termVar <|> termConst <|> termParens

termVar = (TermVar . VarFree) <$> var

termConst = TermConst <$> constInt

termParens = with_parens term
