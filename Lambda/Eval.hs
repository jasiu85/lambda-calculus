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
  TermMul left right -> do
    left' <- local (. \l -> TermMul l right) (evalCallByValue left)
    right' <- local (. \r -> TermMul left' r) (evalCallByValue right)
    case (left', right') of
      (TermConst vl, TermConst vr) -> do
        let reduced = TermConst (vl * vr)
        report reduced
        return reduced
      otherwise -> do
        return $ TermMul left' right'
  TermAdd left right -> do
    left' <- local (. \l -> TermAdd l right) (evalCallByValue left)
    right' <- local (. \r -> TermAdd left' r) (evalCallByValue right)
    case (left', right') of
      (TermConst vl, TermConst vr) -> do
        let reduced = TermConst (vl + vr)
        report reduced
        return reduced
      otherwise -> do
        return $ TermAdd left' right'
  otherwise -> do
    return $ term
