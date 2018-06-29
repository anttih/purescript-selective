module Data.ValidationSelective
  ( VS
  , invalid
  , isValid
  ) where

import Prelude

import Control.Selective (class Selective)
import Data.Either (Either(..), isRight)
import Data.Eq (class Eq1)
import Data.Ord (class Ord1)
import Data.Validation.Semigroup (V, toEither)
import Data.Validation.Semigroup as Validation

newtype VS e a = VS (V e a)

-- | Fail with a validation error.
invalid :: forall err result. err -> VS err result
invalid = VS <<< Validation.invalid

-- | Test whether validation was successful or not.
isValid :: forall err result. VS err result -> Boolean
isValid (VS x) = isRight (toEither x)

derive instance eqVS :: (Eq e, Eq a) => Eq (VS e a)
derive instance eq1VS :: Eq e => Eq1 (VS e)

derive instance ordVS :: (Ord e, Ord a) => Ord (VS e a)
derive instance ord1VS :: Ord e => Ord1 (VS e)

instance showV :: (Show err, Show result) => Show (VS err result) where
  show (VS x) = case (toEither x) of
    Left err -> "invalid (" <> show err <> ")"
    Right result -> "pure (" <> show result <> ")"

derive newtype instance functorVS :: Functor (VS e)
derive newtype instance applyVS :: Semigroup e => Apply (VS e)
derive newtype instance applicativeVS :: Semigroup e => Applicative (VS e)

instance selectiveVS :: Semigroup e => Selective (VS e) where
  handle (VS x) (VS y) = handle' (toEither x) (toEither y)
    where
    handle' x' y' = case x', y' of
      Right (Left a), Right f -> pure (f a)
      Right (Left a), Left e  -> invalid e
      Right (Right b), _      -> pure b
      Left e1, Left e2        -> invalid (e1 <> e2)
      Left e, _               -> invalid e