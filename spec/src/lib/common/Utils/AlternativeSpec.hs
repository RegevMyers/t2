module Utils.AlternativeSpec where

import Utils.Alternative

import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = describe "Spec" $ do
    describe "try" $ do
        prop "defaults to `pure`" $ \(a :: Int) ->
            try (\_ -> Nothing) a == Just a