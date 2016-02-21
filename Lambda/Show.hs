module Lambda.Show() where

import Lambda.Syntax
import Lambda.Named

instance Show Term where
  showsPrec n (TermVar k) =
    shows k
  showsPrec n (TermLambda t) =
    showParen (n > 0) (showString "\\." . showsPrec 0 t)
  showsPrec n (TermApply tf tx) =
    showParen (n > 1) (showsPrec 1 tf . showString " " . showsPrec 1 tx)

instance Show NamedTerm where
  showsPrec n (NamedTermVar s) =
    showString s
  showsPrec n (NamedTermLambda x t) =
    showParen (n > 0) (showString "\\" . showString x . showString "." . showsPrec 0 t)
  showsPrec n (NamedTermApply tf tx) =
    showParen (n > 1) (showsPrec 1 tf . showString " " . showsPrec 1 tx)
