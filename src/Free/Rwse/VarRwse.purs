module Free.Rwse.VarRwse
  ( VarRwse(..)
  , varRwse
  , makeVarRwse
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR, readVar, takeVar, putVar, makeVar)
import Control.Monad.Eff.AVar (AVar)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Free (foldFree)

import Free.Rwse (RwseF(..))

data VarRwse reader writer state = VarRwse reader writer state

varRwse
  :: forall eff f reader writer state.
     Semigroup writer =>
     AVar (VarRwse reader writer state) ->
     (f ~> Aff (avar :: AVAR | eff)) ->
     RwseF f reader writer state Error ~> Aff (avar :: AVAR | eff)
varRwse var interp =
  case _ of
       Ask k -> do
         VarRwse reader _ _ <- readVar var
         pure (k reader)

       Get k -> do
         VarRwse _ _ state <- readVar var
         pure (k state)

       Tell writer' a -> a <$ modifyVar (\(VarRwse reader writer state) -> VarRwse reader (writer <> writer') state)

       Put state a -> a <$ modifyVar (\(VarRwse reader writer _) -> VarRwse reader writer state)

       Throw error k -> k <$> throwError error

       Catch f handle k -> k <$> catchError (foldFree interp f) (foldFree interp <<< handle)
  where
  modifyVar k = do
    a <- takeVar var
    let a' = k a
    putVar a' var

makeVarRwse :: forall eff reader writer state. reader -> writer -> state -> Aff (avar :: AVAR | eff) (AVar (VarRwse reader writer state))
makeVarRwse reader writer state = makeVar (VarRwse reader writer state)
