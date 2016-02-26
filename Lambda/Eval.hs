module Lambda.Eval where

import Control.Applicative

import Lambda.Syntax
import Lambda.Manipulators

evalCallByName term = case term of
  TermApply fun arg -> let fun' = evalCallByName fun in case fun' of
    TermLambda x body -> reduce x body arg
    otherwise -> TermApply fun' arg
  otherwise -> term

evalCallByValue term = case term of
  TermApply fun arg -> let
    fun' = evalCallByValue fun
    arg' = evalCallByValue arg
    in case fun' of
      TermLambda x body -> reduce x body arg'
      otherwise -> TermApply fun' arg'
  otherwise -> term
