module Lambda.Manipulators(bind_params) where

import Control.Monad.Reader
import Data.List(elemIndex)

import Lambda.Syntax

bind_params t =
  runReader (bind_params' t) []
  where
    bind_params' (TermVar vn) = do
      vn' <- bind_var vn
      return $ TermVar vn'
    bind_params' (TermLambda x t) = do
      t' <- local (x:) (bind_params' t)
      return $ TermLambda x t'
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
