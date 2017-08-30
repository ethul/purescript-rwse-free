module Test.Main where

import Prelude

import Control.Alt ((<|>))

import Control.Monad.Aff (launchAff, attempt)
import Control.Monad.Aff.AVar (AVAR, AffAVar, AVar, peekVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Free (Free, hoistFree, liftF, foldFree)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import Control.Monad.Eff.Exception as Exception
import Control.Monad.Free (Free, hoistFree, liftF, foldFree)
import Control.Monad.Except (ExceptT)
import Control.Monad.RWS (RWST, RWSResult(..), runRWST)
import Control.Monad.Trans.Class (class MonadTrans, lift)

import Data.Inject (class Inject, prj)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Functor.Coproduct.Nested as Coproduct
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype, wrap, unwrap)

import Free.Rwse (RwseF, throw, catch, tell)
import Free.Rwse.VarRwse (VarRwse(..), varRwse, makeVarRwse) as Rwse
import Free.Rwse.TransRwse (transRwse) as Rwse

import Partial.Unsafe (unsafePartial)

import Unsafe.Coerce (unsafeCoerce)

import Debug.Trace (spy)

type Baz = Free BazF

newtype BazF a = BazF (Coproduct3 FooF BarF RwseF' a)

type Rwse' = Free RwseF'

newtype RwseF' a = RwseF' (RwseF BazF String String Unit Error a)

type Foo = Free FooF

data FooF a = FooF a

type Bar = Free BarF

data BarF a = BarF a

type M eff = ExceptT Error (RWST String String Unit (Eff eff))

type N eff a = AffAVar eff a

bazM :: forall eff. BazF ~> M eff
bazM fa = unsafePartial $ fromJust $
  (foo <$> prj fa) <|>
  (bar <$> prj fa) <|>
  (rwse <$> prj fa)
  where
  foo :: FooF ~> M eff
  foo _ = pure (unsafeCoerce (spy "M: foo"))

  bar :: BarF ~> M eff
  bar _ = pure (unsafeCoerce (spy "M: bar"))

  rwse :: RwseF' ~> M eff
  rwse = Rwse.transRwse bazM <<< unwrap

bazN :: forall eff. AVar (Rwse.VarRwse String String Unit) -> BazF ~> N eff
bazN var fa = unsafePartial $ fromJust $
  (foo <$> prj fa) <|>
  (bar <$> prj fa) <|>
  (rwse <$> prj fa)
  where
  foo :: FooF ~> N eff
  foo _ = pure (unsafeCoerce (spy "N: foo"))

  bar :: BarF ~> N eff
  bar _ = pure (unsafeCoerce (spy "N: bar"))

  rwse :: RwseF' ~> N eff
  rwse fa' = Rwse.varRwse var (bazN var) (unwrap fa')

injFLift :: forall t f g. (MonadTrans t, Inject f g) => Free f ~> t (Free g)
injFLift = lift <<< injF

bazLog :: String -> Baz Unit
bazLog message = injF tell'
  where
  tell' :: Rwse' Unit
  tell' = hoistFree wrap (tell message)

bazThrow :: forall a. Error -> Baz a
bazThrow error = injF throw'
  where
  throw' :: Rwse' a
  throw' = hoistFree wrap (throw error)

bazCatch :: forall a. Baz a -> (Error -> Baz a) -> Baz a
bazCatch fa handler = injF catch'
  where
  catch' :: Rwse' a
  catch' = hoistFree wrap (catch fa handler)

main :: forall eff. Eff (exception :: EXCEPTION, avar :: AVAR | eff) Unit
main = do
  RWSResult s r w <- runRWST (unwrap (foldFree bazM program)) "Reader" unit

  _ <- pure (spy ("M: " <> w))

  _ <- pure (spy "-----------------------------------------------")

  void $ launchAff $ do
    var <- Rwse.makeVarRwse "Reader" "" unit
    resultM <- attempt $ foldFree (bazN var) program
    Rwse.VarRwse r' w' s' <- peekVar var
    _ <- pure (spy ("N: " <> w'))
    pure unit

  pure unit
  where
  foo' :: Foo Unit
  foo' = liftF (FooF unit)

  bar' :: Bar Unit
  bar' = liftF (BarF unit)

  program :: Baz Unit
  program = do
    injF foo'
    bazLog "one"
    injF bar'
    bazLog "two"
    bazCatch (do injF foo'
                 bazLog "three"
                 void (bazThrow (Exception.error "Error"))
                 bazLog "four"
                 injF bar')
             (\error -> pure (spy error) *> injF foo')
    injF foo'
    pure unit

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
