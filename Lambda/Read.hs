{-# LANGUAGE RankNTypes #-}

module Lambda.Read() where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Char(isAlpha, isDigit, ord)
import Data.Foldable
import Data.Monoid

import Lambda.Syntax

data ParseResult a = ParseSuccess [a]
                   | ParseError
  deriving Show

instance Monoid (ParseResult a) where
  mempty = ParseError
  mappend (ParseSuccess ls) (ParseSuccess rs) = ParseSuccess (ls ++ rs)
  mappend ParseError ParseError = ParseError
  mappend l ParseError = l
  mappend ParseError r = r

instance Foldable ParseResult where
  foldr f z (ParseSuccess as) = foldr f z as
  foldr f z ParseError = z

data Parser m a = Parser {
  runParser :: (forall r . ContT (ParseResult r) (ReaderT String m) a)
}

instance (Monad m) => Functor (Parser m) where
  fmap f p = Parser $ fmap f (runParser p)

instance (Monad m) => Applicative (Parser m) where
  pure a = Parser $ pure a
  pf <*> pa = Parser $ do
    f <- runParser pf
    a <- runParser pa
    return $ f a
 
instance (Monad m) => Monad (Parser m) where
  return = pure
  pa >>= f = Parser $ do
    a <- runParser pa
    let pb = f a
    b <- runParser pb
    return b

instance (Monad m) => Alternative (Parser m) where
  empty = Parser $ ContT $ \k -> return ParseError
  pl <|> pr = Parser $ ContT $ \k -> do
    l <- runContT (runParser pl) k
    r <- runContT (runParser pr) k
    return $ l <> r

instance (Monad m) => MonadPlus (Parser m) where
  mzero = empty
  mplus = (<|>)

parseFail :: (Monad m) => Parser m a
parseFail = empty

parseSeq pa pb = do
  a <- pa
  b <- pb
  return $ (a, b)

parseSepByLeft :: (Monad m) => Parser m a -> Parser m (a -> a -> a) -> Parser m a
parseSepByLeft pa ps = do
  a <- pa
  sas <- psas
  return $ foldl f a sas
  where
    psas = many $ ps `parseSeq` pa
    f l (s, r) = l `s` r

parseSepByRight :: (Monad m) => Parser m a -> Parser m (a -> a -> a) -> Parser m a
parseSepByRight pa ps = do
  ass <- pass
  a <- pa
  return $ foldr f a ass
  where
    pass = many $ pa `parseSeq` ps
    f (l, s) r = l `s` r

parseEof :: (Monad m) => Parser m ()
parseEof = Parser $ ContT $ \k -> do
  s <- ask
  case s of
    [] -> k ()
    _ -> return ParseError

parseGet :: (Monad m) => Parser m Char
parseGet = Parser $ ContT $ \k -> do
  s <- ask
  case s of
    c : _ -> local tail (k c)
    _ -> return ParseError

parseSatisfy :: (Monad m) => Bool -> Parser m ()
parseSatisfy = guard

parseCharSatisfying :: (Monad m) => (Char -> Bool) -> Parser m Char
parseCharSatisfying p = do
  c <- parseGet
  parseSatisfy (p c)
  return c

parseChar :: (Monad m) => Char -> Parser m Char
parseChar c = parseCharSatisfying (== c)

parseString :: (Monad m) => String -> Parser m String
parseString [] = return []
parseString (c : cs) = do
  c' <- parseChar c
  cs' <- parseString cs
  return $ c' : cs'

parseDigit :: (Monad m) => Parser m Integer
parseDigit = do
  c <- parseCharSatisfying isDigit
  return $ toInteger $ (ord c) - (ord '0')

parseInteger :: (Monad m) => Parser m Integer
parseInteger = do
  ds <- some $ parseCharSatisfying isDigit
  let ds' = map (\c -> toInteger $ (ord c) - (ord '0')) ds
  return $ foldl (\i d -> 10 * i + d) 0 ds'
  
parseVarName :: (Monad m) => Parser m String
parseVarName = do
 s <- some $ parseCharSatisfying isAlpha
 return s

parseTermVar :: (Monad m) => Parser m Term
parseTermVar = do
  var <- parseVarName
  return $ TermVar (VarFree var)

parseTermConst :: (Monad m) => Parser m Term
parseTermConst = do
  i <- parseInteger
  return $ TermConst i

parseWithParens :: (Monad m) => Parser m a -> Parser m a
parseWithParens pa = do
  parseChar '('
  a <- pa
  parseChar ')'
  return a

parseTermAtom :: (Monad m) => Parser m Term
parseTermAtom = parseTermVar <|> parseTermConst <|> (parseWithParens parseTerm)

parseTermPow :: (Monad m) => Parser m Term
parseTermPow = parseSepByRight parseTermAtom parsePow where
  parsePow = do
    parseString " ** "
    return $ TermPow

parseTermMul :: (Monad m) => Parser m Term
parseTermMul = parseSepByLeft parseTermPow parseMul where
  parseMul = do
    parseString " * "
    return $ TermMul

parseTermAdd :: (Monad m) => Parser m Term
parseTermAdd = parseSepByLeft parseTermMul parseAdd where
  parseAdd = do
    parseString " + "
    return $ TermAdd

parseTerm :: (Monad m) => Parser m Term
parseTerm = parseTermAdd

parseProgram :: (Monad m) => Parser m Term
parseProgram = do
  t <- parseTerm
  parseEof
  return t

(|>) = flip ($)

parse :: (Monad m) => Parser m a -> (a -> ReaderT String m (ParseResult b)) -> String -> m (ParseResult b)
parse pa finalCont s =
  (runParser pa) |> (`runContT` finalCont) |> (`runReaderT` s)

instance Read Term where
  readsPrec _ s =
    (parse parseTerm finalCont s) |> runIdentity |> toList
    where
      finalCont a = do
        s <- ask
        return $ ParseSuccess [(a, s)]
