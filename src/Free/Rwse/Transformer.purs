module Free.Rwse.Transformer (rwseN) where

import Prelude ((<$>), const)

import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.RWS.Class (MonadRWS, ask, get, put, tell)

import Data.NaturalTransformation (NaturalTransformation())

import Free.Rwse (RwseF(..))

rwseN :: forall reader writer state error monad.
         (MonadRWS reader writer state monad, MonadError error monad) => NaturalTransformation (RwseF reader writer state error) monad
rwseN fa =
  case fa of
       Ask k -> k <$> ask
       Tell writer a -> const a <$> tell writer
       Get k -> k <$> get
       Put state a -> const a <$> put state
       Throw error k -> k <$> throwError error
