module Free.Rwse.VarRwse
  ( VarRwse(..)
  , varRwse
  , makeVarRwse
  ) where

import Prelude

import Control.Monad.Aff.AVar (AffAVar, AVar, peekVar, modifyVar, makeVar')
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Free (foldFree)

import Free.Rwse (RwseF(..))

data VarRwse reader writer state = VarRwse reader writer state

varRwse
  :: forall eff f reader writer state.
     Semigroup writer =>
     AVar (VarRwse reader writer state) ->
     (f ~> AffAVar eff) ->
     RwseF f reader writer state Error ~> AffAVar eff
varRwse var interp =
  case _ of
       Ask k -> do
         VarRwse reader _ _ <- peekVar var
         pure (k reader)

       Get k -> do
         VarRwse _ _ state <- peekVar var
         pure (k state)

       Tell writer' a -> a <$ modifyVar (\(VarRwse reader writer state) -> VarRwse reader (writer <> writer') state) var

       Put state a -> a <$ modifyVar (\(VarRwse reader writer _) -> VarRwse reader writer state) var

       Throw error k -> k <$> throwError error

       Catch f handle k -> k <$> catchError (foldFree interp f) (foldFree interp <<< handle)

makeVarRwse :: forall eff reader writer state. reader -> writer -> state -> AffAVar eff (AVar (VarRwse reader writer state))
makeVarRwse reader writer state = makeVar' (VarRwse reader writer state)
