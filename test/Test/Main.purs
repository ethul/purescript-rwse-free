module Test.Main (main) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Free (Free, hoistFree, liftF, foldFree)
import Control.Monad.Except (ExceptT)
import Control.Monad.RWS (RWST, RWSResult(..), runRWST)
import Control.Monad.Trans.Class (class MonadTrans, lift)

import Effect.Aff (Aff, launchAff, attempt)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Exception as Exception

import Data.Functor.Coproduct.Inject (class Inject, inj)
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

type M = ExceptT Error (RWST String String Unit Effect)

type N a = Aff a

bazM :: BazF ~> M
bazM fa = unsafePartial $ fromJust $
  (foo <$> (Coproduct.at1 Nothing Just $ unwrap fa)) <|>
  (bar <$> (Coproduct.at2 Nothing Just $ unwrap fa)) <|>
  (rwse <$> (Coproduct.at3 Nothing Just $ unwrap fa))
  where
  foo :: FooF ~> M
  foo _ = pure (unsafeCoerce (spy "M: foo"))

  bar :: BarF ~> M
  bar _ = pure (unsafeCoerce (spy "M: bar"))

  rwse :: RwseF' ~> M
  rwse = Rwse.transRwse bazM <<< unwrap

bazN :: AVar (Rwse.VarRwse String String Unit) -> BazF ~> N
bazN var fa = unsafePartial $ fromJust $
  (foo <$> (Coproduct.at1 Nothing Just $ unwrap fa)) <|>
  (bar <$> (Coproduct.at2 Nothing Just $ unwrap fa)) <|>
  (rwse <$> (Coproduct.at3 Nothing Just $ unwrap fa))
  where
  foo :: FooF ~> N
  foo _ = pure (unsafeCoerce (spy "N: foo"))

  bar :: BarF ~> N
  bar _ = pure (unsafeCoerce (spy "N: bar"))

  rwse :: RwseF' ~> N
  rwse fa' = Rwse.varRwse var (bazN var) (unwrap fa')

injFLift :: forall t f g. MonadTrans t => Inject f g => Free f ~> t (Free g)
injFLift = lift <<< injF

bazLog :: String -> Baz Unit
bazLog message =
  hoistFree (wrap <<< Coproduct.in3) tell'
  where
  tell' :: Rwse' Unit
  tell' = hoistFree wrap (tell message)

bazThrow :: forall a. Error -> Baz a
bazThrow error =
  hoistFree (wrap <<< Coproduct.in3) throw'
  where
  throw' :: Rwse' a
  throw' = hoistFree wrap (throw error)

bazCatch :: forall a. Baz a -> (Error -> Baz a) -> Baz a
bazCatch fa handler =
  hoistFree (wrap <<< Coproduct.in3) catch'
  where
  catch' :: Rwse' a
  catch' = hoistFree wrap (catch fa handler)

main :: Effect Unit
main = do
  RWSResult s r w <- runRWST (unwrap (foldFree bazM program)) "Reader" unit

  _ <- pure $ spy "M:" w

  _ <- pure $ spy "-----------------------------------------------" ""

  void $ launchAff $ do
    var <- Rwse.makeVarRwse "Reader" "" unit
    resultM <- attempt $ foldFree (bazN var) program
    Rwse.VarRwse r' w' s' <- AVar.read var
    _ <- pure $ spy "N:" w'
    pure unit

  pure unit
  where
  foo' :: Foo Unit
  foo' = liftF (FooF unit)

  bar' :: Bar Unit
  bar' = liftF (BarF unit)

  program :: Baz Unit
  program = do
    injFoo foo'
    bazLog "one"
    injBar bar'
    bazLog "two"
    bazCatch (do injFoo foo'
                 bazLog "three"
                 void (bazThrow (Exception.error "Error"))
                 bazLog "four"
                 injBar bar')
             (\error -> pure (spy "Error" error) *> injFoo foo')
    injFoo foo'
    pure unit

injF :: forall f g. Inject f g => Free f ~> Free g
injF = hoistFree inj

injFoo :: forall a. Foo a -> Baz a
injFoo = hoistFree (wrap <<< Coproduct.in1)

injBar :: forall a. Bar a -> Baz a
injBar = hoistFree (wrap <<< Coproduct.in2)

derive instance newtypeBazF :: Newtype (BazF a) _

derive instance newtypeRwseF' :: Newtype (RwseF' a) _
