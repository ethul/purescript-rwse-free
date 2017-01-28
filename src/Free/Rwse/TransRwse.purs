module Free.Rwse.TransRwse (transRwse) where

import Prelude

import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Control.Monad.Free (foldFree)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State.Class (class MonadState, get, put)
import Control.Monad.Writer.Class (class MonadTell, tell)

import Free.Rwse (RwseF(..))

transRwse
  :: forall f reader writer state error monad.
     ( MonadAsk reader monad
     , MonadState state monad
     , MonadTell writer monad
     , MonadError error monad
     , MonadRec monad
     ) => (f ~> monad) -> RwseF f reader writer state error ~> monad
transRwse interp fa =
  case fa of
       Ask k -> k <$> ask
       Tell writer a -> const a <$> tell writer
       Get k -> k <$> get
       Put state a -> const a <$> put state
       Throw error k -> k <$> throwError error
       Catch f handle k -> k <$> catchError (foldFree interp f) (foldFree interp <<< handle)
