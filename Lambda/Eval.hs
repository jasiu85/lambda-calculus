module Lambda.Eval where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Lambda.Syntax
import Lambda.Manipulators

reportSingleStep newPartialTerm = do
  oldFullTerm <- get
  lens <- ask
  let newFullTerm = lensSet lens newPartialTerm oldFullTerm
  tell [newFullTerm]
  put newFullTerm
  return newPartialTerm

evalSingleStep term = reportSingleStep $ step term where
  step (TermApply (TermLambda _ body) arg) =
    betaReduce body arg
  step (TermMul (TermConst left) (TermConst right)) =
    TermConst (left * right)
  step (TermAdd (TermConst left) (TermConst right)) =
    TermConst (left + right)

evalCallByValue term = eval term where
  eval (TermApply fun arg) = do
    fun' <- local (`lensCompose` lensTermApplyFun) (eval fun)
    arg' <- local (`lensCompose` lensTermApplyArg) (eval arg)
    term' <- evalSingleStep $ TermApply fun' arg'
    eval term'
  eval (TermMul left right) = do
    left' <- local (`lensCompose` lensTermMulLeft) (eval left)
    right' <- local (`lensCompose` lensTermMulRight) (eval right)
    evalSingleStep $ TermMul left' right'
  eval (TermAdd left right) = do
    left' <- local (`lensCompose` lensTermAddLeft) (eval left)
    right' <- local (`lensCompose` lensTermAddRight) (eval right)
    evalSingleStep $ TermAdd left' right'
  eval term = do
    return term

evalCallByName term = eval term where
  eval (TermApply fun arg) = do
    fun' <- local (`lensCompose` lensTermApplyFun) (eval fun)
    term' <- evalSingleStep $ TermApply fun' arg
    eval term'
  eval (TermMul left right) = do
    left' <- local (`lensCompose` lensTermMulLeft) (eval left)
    right' <- local (`lensCompose` lensTermMulRight) (eval right)
    evalSingleStep $ TermMul left' right'
  eval (TermAdd left right) = do
    left' <- local (`lensCompose` lensTermAddLeft) (eval left)
    right' <- local (`lensCompose` lensTermAddRight) (eval right)
    evalSingleStep $ TermAdd left' right'
  eval term = do
    return term
