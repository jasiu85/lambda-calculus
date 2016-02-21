module Lambda.Named(NamedTerm, to_named, from_named) where

import Data.List(elemIndex)
import Data.Maybe(fromJust)

import Lambda.Syntax

data NamedTerm = NamedTermVar String
               | NamedTermLambda String NamedTerm
               | NamedTermApply NamedTerm NamedTerm
  deriving (Eq, Show)

to_named fv_names bv_names t =
  to_named' 0 t
  where
    to_named' lnl (TermVar k) =
      NamedTermVar (v_name lnl k)
    to_named' lnl (TermLambda t) =
      NamedTermLambda (bv_names !! lnl) (to_named' (lnl + 1) t)
    to_named' lnl (TermApply tf tx) =
      NamedTermApply (to_named' lnl tf) (to_named' lnl tx)
    v_name lnl k =
      if k < lnl
      then bv_names !! (lnl - k - 1)
      else fv_names !! (k - lnl)

from_named fv_names t =
  from_named' [] t
  where
    from_named' bv_names (NamedTermVar x) =
      TermVar (v_idx bv_names x)
    from_named' bv_names (NamedTermLambda x t) =
      TermLambda (from_named' (x : bv_names) t)
    from_named' bv_names (NamedTermApply tf tx) =
      TermApply (from_named' bv_names tf) (from_named' bv_names tx)
    v_idx bv_names x =
      case elemIndex x bv_names of
        Just k  -> k
        Nothing -> (fromJust $ elemIndex x fv_names) + (length bv_names)
