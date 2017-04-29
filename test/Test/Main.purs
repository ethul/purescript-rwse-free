module Test.Main where

import Control.Alt ((<|>))

import Control.Monad.Eff (Eff)
import Control.Monad.Free (Free, hoistFree, liftF, foldFree)
import Control.Monad.Except (ExceptT)
import Control.Monad.RWS (RWST, runRWST)
import Control.Monad.Trans.Class (class MonadTrans, lift)

import Data.Inject (class Inject, inj, prj)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Functor.Coproduct.Nested as Coproduct
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype, wrap, unwrap)

import Prelude

import Free.Rwse (RwseF, throw, catch)
import Free.Rwse.TransRwse as Rwse

import Partial.Unsafe (unsafePartial)

import Unsafe.Coerce (unsafeCoerce)

import Debug.Trace (spy)

type Baz = Free BazF

newtype BazF a = BazF (Coproduct3 FooF BarF RwseF' a)

type Rwse' = Free RwseF'

newtype RwseF' a = RwseF' (RwseF BazF String String Unit String a)

type Foo = Free FooF

data FooF a = FooF a

type Bar = Free BarF

data BarF a = BarF a

type M eff = ExceptT String (RWST String String Unit (Eff eff))

baz :: forall eff. BazF ~> M eff
baz fa = unsafePartial $ fromJust $
         (foo <$> prj fa) <|>
         (bar <$> prj fa) <|>
         (rwse <$> prj fa)
  where
  foo :: FooF ~> M eff
  foo _ = pure (unsafeCoerce (spy "foo"))

  bar :: BarF ~> M eff
  bar _ = pure (unsafeCoerce (spy "bar"))

  rwse :: RwseF' ~> M eff
  rwse = Rwse.transRwse baz <<< unwrap

injFLift :: forall t f g. MonadTrans t => Inject f g => Free f ~> t (Free g)
injFLift = lift <<< injF

bazThrow :: forall a. String -> Baz a
bazThrow error = injF throw'
  where
  throw' :: Rwse' a
  throw' = hoistFree wrap (throw error)

bazCatch :: forall a. Baz a -> (String -> Baz a) -> Baz a
bazCatch fa handler = injF catch'
  where
  catch' :: Rwse' a
  catch' = hoistFree wrap (catch fa handler)

main :: forall eff. Eff eff Unit
main = do
  result <- runRWST (unwrap (foldFree baz program)) "Reader" unit
  pure unit
  where
  foo' :: Foo Unit
  foo' = liftF (FooF unit)

  bar' :: Bar Unit
  bar' = liftF (BarF unit)

  program :: Baz Unit
  program = do
    injF foo'
    injF bar'
    bazCatch (do injF foo'
                 void (bazThrow "Error")
                 injF bar')
             (\error -> pure (spy error) *> injF foo')
    injF foo'
    pure unit

injF :: forall f g. Inject f g => Free f ~> Free g
injF = hoistFree inj

instance injectFooFBazF :: Inject FooF BazF where
  inj = wrap <<< Coproduct.in1
  prj = Coproduct.at1 Nothing Just <<< unwrap

instance injectBarFBazF :: Inject BarF BazF where
  inj = wrap <<< Coproduct.in2
  prj = Coproduct.at2 Nothing Just <<< unwrap

instance injectRwseFBazF :: Inject RwseF' BazF where
  inj = wrap <<< Coproduct.in3
  prj = Coproduct.at3 Nothing Just <<< unwrap

derive instance newtypeBazF :: Newtype (BazF a) _

derive instance newtypeRwseF' :: Newtype (RwseF' a) _
