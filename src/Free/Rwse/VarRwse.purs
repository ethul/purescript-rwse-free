module Free.Rwse.VarRwse
  ( VarRwse(..)
  , varRwse
  , makeVarRwse
  ) where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Free (foldFree)

import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Exception (Error)

import Free.Rwse (RwseF(..))

data VarRwse r w s = VarRwse r w s

varRwse
  :: forall f r w s
   . Semigroup w
  => AVar (VarRwse r w s)
  -> (f ~> Aff)
  -> RwseF f r w s Error
  ~> Aff
varRwse var interp =
  case _ of
       Ask k -> do
         VarRwse r _ _ <- AVar.read var
         pure (k r)

       Get k -> do
         VarRwse _ _ s <- AVar.read var
         pure (k s)

       Tell w' a -> a <$ modifyVar (\(VarRwse r w s) -> VarRwse r (w <> w') s)

       Put s a -> a <$ modifyVar (\(VarRwse r w _) -> VarRwse r w s)

       Throw e k -> k <$> throwError e

       Catch f handle k -> k <$> catchError (foldFree interp f) (foldFree interp <<< handle)
  where
  modifyVar k = do
    a <- AVar.take var
    let a' = k a
    AVar.put a' var

makeVarRwse
  :: forall r w s
   . r
  -> w
  -> s
  -> Aff (AVar (VarRwse r w s))
makeVarRwse r w s =
  AVar.new $ VarRwse r w s
