module Test.Util
  ( exampleProperty
  ) where

import           Prelude
import           Hedgehog (Property, PropertyT)
import           Hedgehog (property, withTests, withShrinks)

exampleProperty :: PropertyT IO () -> Property
exampleProperty = withShrinks 0 . withTests 1 . property

