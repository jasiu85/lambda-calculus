{-# LANGUAGE RankNTypes #-}

module Lambda.Manipulators where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Functor
import Data.List(elemIndex)

import Lambda.Syntax

type TermLens = forall f. Functor f => (Term -> f Term) -> (Term -> f Term)

lensGet :: TermLens -> Term -> Term
lensGet lens fullTerm =
  getConst $ lens Const fullTerm

lensSet :: TermLens -> Term -> Term -> Term
lensSet lens subTerm fullTerm =
  runIdentity $ lens (const $ Identity subTerm) fullTerm

lensTermApplyFun f (TermApply fun arg) =
  (\fun' -> TermApply fun' arg) <$> (f fun)

lensTermApplyArg f (TermApply fun arg) =
  (\arg' -> TermApply fun arg') <$> (f arg)

lensTermMulLeft f (TermMul left right) =
  (\left' -> TermMul left' right) <$> (f left)

lensTermMulRight f (TermMul left right) =
  (\right' -> TermMul left right') <$> (f right)

lensTermAddLeft f (TermAdd left right) =
  (\left' -> TermAdd left' right) <$> (f left)

lensTermAddRight f (TermAdd left right) =
  (\right' -> TermAdd left right') <$> (f right)

lensTermFunBody f (TermLambda var body) =
  (\body' -> TermLambda var body') <$> (f body)

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

betaReduce body arg = body'' where
  arg' = shift 0 1 arg
  body' = substitute 0 arg' body
  body'' = shift 0 (-1) body'
