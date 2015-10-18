module Free.Rwse
  ( Rwse()
  , RwseF(..)
  , ask
  , tell
  , get
  , put
  , modify
  , throw
  ) where

import Prelude (Unit(), (>>=), (<<<), id, unit)

import Control.Monad.Free (Free(), liftF)

type Rwse reader writer state error = Free (RwseF reader writer state error)

data RwseF reader writer state error a
  = Ask (reader -> a)
  | Tell writer a
  | Get (state -> a)
  | Put state a
  | Throw error (a -> a)

ask :: forall reader writer state error. Rwse reader writer state error reader
ask = liftF (Ask id)

tell :: forall reader writer state error. writer -> Rwse reader writer state error Unit
tell w = liftF (Tell w unit)

get :: forall reader writer state error. Rwse reader writer state error state
get = liftF (Get id)

put :: forall reader writer state error. state -> Rwse reader writer state error Unit
put s = liftF (Put s unit)

modify :: forall reader writer state error. (state -> state) -> Rwse reader writer state error Unit
modify k = get >>= (put <<< k)

throw :: forall reader writer state error a. error -> Rwse reader writer state error a
throw e = liftF (Throw e id)
