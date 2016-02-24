module Lambda.Show(DeBruijn(..)) where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
 
import Lambda.Syntax

newtype DeBruijn = DeBruijn Term

instance Show DeBruijn where
  showsPrec _ (DeBruijn t) = runIdentity $ execWriterT $ show' t where
    show' (TermVar vn) = show_var vn
    show' (TermLambda x t) = do
      tell $ showString "\\"
      tell $ showString x
      tell $ showString "."
      tell $ showString "("
      show' t
      tell $ showString ")"
    show' (TermApply tf tx) = do
      tell $ showString "("
      show' tf
      tell $ showString " "
      show' tx
      tell $ showString ")"
    show_var (VarFree s) = tell $ showString s
    show_var (VarBound k) = tell $ shows k 

instance Show Term where
  showsPrec _ t = runIdentity $ execWriterT $ (`runReaderT `[]) $ show' t where
    show' (TermVar vn) = show_var vn
    show' (TermLambda x t) = do
      tell $ showString "\\"
      tell $ showString x
      tell $ showString "."
      tell $ showString "("
      local (x:) (show' t)
      tell $ showString ")"
    show' (TermApply tf tx) = do
      tell $ showString "("
      show' tf
      tell $ showString " "
      show' tx
      tell $ showString ")"
    show_var (VarFree s) = tell $ showString s
    show_var (VarBound k) = do
      binders <- ask
      tell $ showString (binders !! k)
