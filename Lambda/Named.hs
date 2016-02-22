module Lambda.Named(NamedTerm(..), to_named, from_named) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Char(chr,ord)
import Data.List(elemIndex)
import Data.Maybe(fromJust)

import Lambda.Syntax

data NamedTerm = NamedTermVar String
               | NamedTermLambda String NamedTerm
               | NamedTermApply NamedTerm NamedTerm
  deriving (Eq)

to_named t =
  runReader (to_named' t) 0
  where
    to_named' (TermVar k) = do
      k' <- var_name k
      return $ NamedTermVar k'
    to_named' (TermLambda t) = do
      x' <- local (+1) (var_name 0)
      t' <- local (+1) (to_named' t)
      return $ NamedTermLambda x' t'
    to_named' (TermApply tf tx) = do
      tf' <- to_named' tf
      tx' <- to_named' tx
      return $ NamedTermApply tf' tx'
    var_name k = do
      d <- ask
      if k < d
      then return $ bvar_name (d - k - 1)
      else return $ fvar_name (k - d)
      where
        bvar_name k
          | k < 3 = [chr ((ord 'x') + k)]
	  | k < 7 = [chr ((ord 't') + k - 3)]
	  | otherwise = "bv" ++ (show k)
        fvar_name k 
          | k < 10 = [chr ((ord 'a') + k)]
          | otherwise = "fv" ++ (show k)

from_named t =
  fst $ runState (from_named' t) ([], [])
  where
    from_named' (NamedTermVar x) = do
      x' <- v_idx x
      return $ TermVar x'
    from_named' (NamedTermLambda x t) = do
      push_bvar x
      t' <- from_named' t
      pop_bvar x
      return $ TermLambda t'
    from_named' (NamedTermApply tf tx) = do
      tf' <- from_named' tf
      tx' <- from_named' tx
      return $ TermApply tf' tx'
    push_bvar x = do
      (bvars, fvars) <- get
      put (x : bvars, fvars)
    pop_bvar x = do
      (x : bvars, fvars) <- get
      put (bvars, fvars)
    v_idx x = do
      (bvars, _) <- get
      case elemIndex x bvars of
        Just k -> return k
        Nothing -> do
          (bvars, fvars) <- get
          case elemIndex x fvars of
            Just k -> return $ (length bvars) + k
            Nothing -> do
             let fvars' = fvars ++ [x]
             put (bvars, fvars')
             return $ (length bvars) + (length fvars') - 1 
