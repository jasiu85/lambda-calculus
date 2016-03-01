module Lambda.Manipulators(bind_params, reduce) where

import Control.Monad.Reader
import Data.List(elemIndex)

import Lambda.Syntax

bind_params t =
  runReader (bind_params' t) []
  where
    bind_params' (TermVar vn) = do
      vn' <- bind_var vn
      return $ TermVar vn'
    bind_params' (TermConst val) = do
      return $ TermConst val
    bind_params' (TermLambda x t) = do
      t' <- local (x:) (bind_params' t)
      return $ TermLambda x t'
    bind_params' (TermAdd l r) = do
      l' <- bind_params' l
      r' <- bind_params' r
      return $ TermAdd l' r'
    bind_params' (TermMul l r) = do
      l' <- bind_params' l
      r' <- bind_params' r
      return $ TermMul l' r'
    bind_params' (TermApply tf tx) = do
      tf' <- bind_params' tf
      tx' <- bind_params' tx
      return $ TermApply tf' tx'

    bind_var (VarFree s) = do
      binders <- ask
      case elemIndex s binders of
        Just k -> return $ VarBound k
        Nothing -> return $ VarFree s
    bind_var (VarBound k) = do
      return $ VarBound k

shift c d t@(TermVar (VarFree s)) = t
shift c d t@(TermVar (VarBound k)) =
  if k < c
  then t
  else TermVar $ VarBound $ k + d
shift c d t@(TermConst _) = t
shift c d (TermLambda x t) = TermLambda x t' where
  t' = shift (c + 1) d t
shift c d (TermAdd l r) = TermAdd l' r' where
  l' = shift c d l
  r' = shift c d r
shift c d (TermMul l r) = TermMul l' r' where
  l' = shift c d l
  r' = shift c d r
shift c d (TermApply tf tx) = TermApply tf' tx' where
  tf' = shift c d tf
  tx' = shift c d tx

substitute i u t@(TermVar (VarFree s)) = t
substitute i u t@(TermVar (VarBound k)) = if i == k then u else t
substitute i u t@(TermConst _) = t
substitute i u (TermLambda x t) = TermLambda x t' where
  t' = substitute (i + 1) u' t
  u' = shift 0 1 u
substitute i u (TermAdd l r) = TermAdd l' r' where
  l' = substitute i u l
  r' = substitute i u r
substitute i u (TermMul l r) = TermMul l' r' where
  l' = substitute i u l
  r' = substitute i u r
substitute i u (TermApply tf tx) = TermApply tf' tx' where
  tf' = substitute i u tf
  tx' = substitute i u tx

reduce x body arg = body'' where
  arg' = shift 0 1 arg
  body' = substitute 0 arg' body
  body'' = shift 0 (-1) body'
