module Lambda.Show(NamedTerm(..), DeBruijnTerm(..), AstTerm(..)) where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
 
import Lambda.Syntax

newtype NamedTerm = NamedTerm Term

newtype DeBruijnTerm = DeBruijnTerm Term

newtype AstTerm = AstTerm Term

instance Show NamedTerm where
  showsPrec _ (NamedTerm t) = showTerm showVarNamed t

instance Show DeBruijnTerm where
  showsPrec _ (DeBruijnTerm t) = showTerm showVarDeBruijn t

instance Show AstTerm where
  showsPrec _ (AstTerm t) = execWriter $ showAstTerm t where
    showAstTerm (TermVar vn) = do
      tell $ showString "(TermVar "
      showAstVar vn
      tell $ showString ")"
    showAstTerm (TermLambda x t) = do
      tell $ showString "(TermLambda "
      tell $ showString x
      tell $ showString " "
      showAstTerm t
      tell $ showString ")"
    showAstTerm (TermApply tf tx) = do
      tell $ showString "(TermApply "
      showAstTerm tf
      tell $ showString " "
      showAstTerm tx
      tell $ showString ")"
    showAstVar (VarFree s) = do
      tell $ showString "(VarFree "
      tell $ showString s
      tell $ showString ")"
    showAstVar (VarBound k) = do
      tell $ showString "(VarBound "
      tell $ shows k
      tell $ showString ")"

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

