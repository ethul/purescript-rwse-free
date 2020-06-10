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

import Prelude

import Control.Monad.Free (Free, liftF)

type Rwse f r w s e = Free (RwseF f r w s e)

data RwseF f r w s e a
  = Ask (r -> a)
  | Tell w a
  | Get (s -> a)
  | Put s a
  | Throw e (a -> a)
  | Catch (Free f a) (e -> Free f a) (a -> a)

instance functorRwseF :: Functor f => Functor (RwseF f r w s e) where
  map k =
    case _ of
         Ask a -> Ask (k <<< a)
         Tell a b -> Tell a (k b)
         Get a -> Get (k <<< a)
         Put a b -> Put a (k b)
         Throw a _ -> Throw a identity
         Catch a b _ -> Catch (k <$> a) (map k <<< b) identity

ask
  :: forall f r w s e
   . Rwse f r w s e r
ask = liftF (Ask identity)

tell
  :: forall f r w s e
   . w
  -> Rwse f r w s e Unit
tell w = liftF (Tell w unit)

get
  :: forall f r w s e
   . Rwse f r w s e s
get = liftF (Get identity)

put
  :: forall f r w s e
   . s
  -> Rwse f r w s e Unit
put s = liftF (Put s unit)

modify
  :: forall f r w s e
   . (s -> s)
  -> Rwse f r w s e Unit
modify k = get >>= (put <<< k)

throw
  :: forall f r w s e a
   . e
  -> Rwse f r w s e a
throw e = liftF (Throw e identity)

catch
  :: forall f r w s e a
   . Free f a
  -> (e -> Free f a)
  -> Rwse f r w s e a
catch f k = liftF (Catch f k identity)
