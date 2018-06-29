module Test.Main where

import Prelude

import Control.Selective (class Selective, ifS)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.ValidationSelective (VS, invalid)
import Effect (Effect)
import Effect.Console as Console

type Radius = Int
type Width  = Int
type Height = Int

data Shape = Circle Radius | Rectangle Width Height

derive instance genericShape :: Generic Shape _

instance showShape :: Show Shape where
  show = genericShow

shape :: forall f. Selective f => f Boolean -> f Radius -> f Width -> f Height -> f Shape
shape s r w h = ifS s (Circle <$> r) (Rectangle <$> w <*> h)

main :: Effect Unit
main = do
  Console.logShow $ ifS (pure true) (pure 1) (pure 2) :: VS (Array String) Int
  Console.logShow $ shape
    (pure false)
    (invalid ["no radius"])
    (invalid ["no width"]) (invalid ["no height"])