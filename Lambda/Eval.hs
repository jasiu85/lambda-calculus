module Lambda.Eval where

import Control.Monad.Reader
import Control.Monad.Writer

import Lambda.Syntax
import Lambda.Manipulators

report argTerm = do
  contextTerm <- ask
  tell $ [contextTerm argTerm]

evalCallByName term = case term of
  TermApply fun arg -> do
    fun' <- local (. \f -> TermApply f arg) (evalCallByName fun)
    case fun' of
      TermLambda x body -> do
        let reduced = reduce x body arg
        report reduced
        evalCallByName reduced
      otherwise -> do
        return $ TermApply fun' arg
  otherwise -> do
    return $ term

evalCallByValue term = case term of
  TermApply fun arg -> do
    fun' <- local (. \f -> TermApply f arg) (evalCallByValue fun)
    arg' <- local (. \a -> TermApply fun' a) (evalCallByValue arg)
    case fun' of
      TermLambda x body -> do 
        let reduced = reduce x body arg'
        report reduced
        evalCallByValue reduced
      otherwise -> do
        return $ TermApply fun' arg'
  otherwise -> do
    return $ term
