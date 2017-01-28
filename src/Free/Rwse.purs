module Free.Rwse
  ( Rwse
  , RwseF(..)
  , ask
  , tell
  , get
  , put
  , modify
  , throw
  , catch
  ) where

import Prelude (Unit, (>>=), (<<<), id, unit)

import Control.Monad.Free (Free, liftF)

type Rwse f reader writer state error = Free (RwseF f reader writer state error)

data RwseF f reader writer state error a
  = Ask (reader -> a)
  | Tell writer a
  | Get (state -> a)
  | Put state a
  | Throw error (a -> a)
  | Catch (Free f a) (error -> Free f a) (a -> a)

ask :: forall f reader writer state error. Rwse f reader writer state error reader
ask = liftF (Ask id)

tell :: forall f reader writer state error. writer -> Rwse f reader writer state error Unit
tell w = liftF (Tell w unit)

get :: forall f reader writer state error. Rwse f reader writer state error state
get = liftF (Get id)

put :: forall f reader writer state error. state -> Rwse f reader writer state error Unit
put s = liftF (Put s unit)

modify :: forall f reader writer state error. (state -> state) -> Rwse f reader writer state error Unit
modify k = get >>= (put <<< k)

throw :: forall f reader writer state error a. error -> Rwse f reader writer state error a
throw e = liftF (Throw e id)

catch :: forall f reader writer state error a. Free f a -> (error -> Free f a) -> Rwse f reader writer state error a
catch f k = liftF (Catch f k id)
