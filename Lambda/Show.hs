module Lambda.Show(NamedTerm(..), DeBruijnTerm(..)) where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
 
import Lambda.Syntax

newtype NamedTerm = NamedTerm Term

newtype DeBruijnTerm = DeBruijnTerm Term

instance Show NamedTerm where
  showsPrec _ (NamedTerm t) = showTerm showVarNamed t

instance Show DeBruijnTerm where
  showsPrec _ (DeBruijnTerm t) = showTerm showVarDeBruijn t

showTerm showVar =
  runIdentity . execWriterT . (`runReaderT` []) . showTerm'
  where
  showTerm' (TermVar vn) = showVar vn
  showTerm' (TermLambda x t) = do
    tell $ showString "\\"
    tell $ showString x
    tell $ showString "."
    local (x:) $ showTerm' t
  showTerm' (TermApply tf tx) = do
    tell $ showString "("
    showTerm' tf
    tell $ showString " "
    showTerm' tx
    tell $ showString ")"

showVarNamed (VarFree s) = tell $ showString s
showVarNamed (VarBound k) = do
  binders <- ask
  tell $ showString (binders !! k)

showVarDeBruijn (VarFree s) = tell $ showString s
showVarDeBruijn (VarBound k) = tell $ shows k

