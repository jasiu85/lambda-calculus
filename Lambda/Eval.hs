module Lambda.Eval where

import Control.Monad.Reader
import Control.Monad.Writer

import Lambda.Syntax
import Lambda.Manipulators

evalCallByName term = eval term where
  eval (TermApply fun arg) = term' where
    fun' = eval fun
    term' = eval $ reduce $ TermApply fun' arg
  eval (TermMul left right) = term' where
    left' = eval left
    right' = eval right
    term' = reduce $ TermMul left' right'
  eval (TermAdd left right) = term' where
    left' = eval left
    right' = eval right
    term' = reduce $ TermAdd left' right'
  eval term = term
  reduce (TermApply (TermLambda _ body) arg) =
    betaReduce body arg
  reduce (TermMul (TermConst left) (TermConst right)) =
    TermConst (left * right)
  reduce (TermAdd (TermConst left) (TermConst right)) =
    TermConst (left + right)
  reduce term = term

evalCallByValue term = eval term where
  eval (TermApply fun arg) = term' where
    fun' = eval fun
    arg' = eval arg
    term' = eval $ reduce $ TermApply fun' arg'
  eval (TermMul left right) = term' where
    left' = eval left
    right' = eval right
    term' = reduce $ TermMul left' right'
  eval (TermAdd left right) = term' where
    left' = eval left
    right' = eval right
    term' = reduce $ TermAdd left' right'
  eval term = term
  reduce (TermApply (TermLambda _ body) arg) =
    betaReduce body arg
  reduce (TermMul (TermConst left) (TermConst right)) =
    TermConst (left * right)
  reduce (TermAdd (TermConst left) (TermConst right)) =
    TermConst (left + right)
  reduce term = term
