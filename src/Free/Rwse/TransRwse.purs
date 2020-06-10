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
  :: forall f r w s e m
   . MonadAsk r m
  => MonadState s m
  => MonadTell w m
  => MonadError e m
  => MonadRec m
  => (f ~> m)
  -> RwseF f r w s e
  ~> m
transRwse interp fa =
  case fa of
       Ask k -> k <$> ask
       Tell writer a -> const a <$> tell writer
       Get k -> k <$> get
       Put state a -> const a <$> put state
       Throw error k -> k <$> throwError error
       Catch f handle k -> k <$> catchError (foldFree interp f) (foldFree interp <<< handle)
