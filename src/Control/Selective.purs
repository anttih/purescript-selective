module Control.Selective where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe, maybe)
import Effect (Effect)

-- | `Selective` is a typeclass somewhere between `Applicative` and `Monad`.
-- | It adds the ability to have conditionals on top of `Applicative`s yet
-- | still allows static analyzis of the program.
-- |
-- | We can leverage the default Monad implementation for many of the instances
-- | since they are Monads anyway. We still get the benefits though.
class Applicative f <= Selective f where
  handle :: forall a b. f (Either a b) -> f (a -> b) -> f b

infixl 4 handle as <*?

instance selectiveIO :: Selective Effect where
  handle = handleM

instance selectiveArray :: Selective Array where
  handle = handleM

instance selectiveMaybe :: Selective Maybe where
  handle = handleM


select :: forall f a b c. Selective f => f (Either a b) -> f (a -> c) -> f (b -> c) -> f c
select i t e = map (map Left) i <*? map (map Right) t <*? e

handleM :: forall f a b. Monad f => f (Either a b) -> f (a -> b) -> f b
handleM i t = do
  res <- i
  case res of
    Left a -> t >>= \f -> pure (f a)
    Right b -> pure b

ifS :: forall f a. Selective f => f Boolean -> f a -> f a -> f a
ifS i t e = select (map (bool (Left unit) (Right unit)) i) (map const t) (map const e)
  where
  bool x y test = if test then x else y

whenS :: forall f. Selective f => f Boolean -> f Unit -> f Unit
whenS i t = ifS i t (pure unit)

fromMaybeS :: forall f a. Selective f => f a -> f (Maybe a) -> f a
fromMaybeS t i = handle ((maybe (Left unit) Right) <$> i) (const <$> t)

orElse :: forall f e a. Selective f => Semigroup e => f (Either e a) -> f (Either e a) -> f (Either e a)
orElse i t = handle (map (map Right) i) (map (\t' e -> lmap (e <> _) t') t)

orS :: forall f. Selective f => f Boolean -> f Boolean -> f Boolean
orS test alt = ifS test (pure true) alt

infixl 4 orS as <||>

andS :: forall f. Selective f => f Boolean -> f Boolean -> f Boolean
andS test t = ifS test t (pure false)

infixl 4 andS as <&&>
