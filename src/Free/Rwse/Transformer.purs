module Free.Rwse.Transformer (rwse) where

import Prelude (type (~>), (<$>), const)

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.State.Class (class MonadState, get, put)
import Control.Monad.Writer.Class (class MonadTell, tell)

import Free.Rwse (RwseF(..))

rwse :: forall reader writer state error monad. (MonadAsk reader monad, MonadState state monad, MonadTell writer monad, MonadError error monad) => RwseF reader writer state error ~> monad
rwse fa =
  case fa of
       Ask k -> k <$> ask
       Tell writer a -> const a <$> tell writer
       Get k -> k <$> get
       Put state a -> const a <$> put state
       Throw error k -> k <$> throwError error
