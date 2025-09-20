module Utils.CharSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((==>))

import Utils.Char (toChar)

spec :: Spec
spec = describe "Spec" $ do
    describe "toChar" $ do
        it "translates 0 to 'A'" $ do
            toChar 0 `shouldBe` Just 'A'
        it "translates [0..25] to ['A'..'Z']" $ do
            mapM toChar [ 0 .. 25 ] `shouldBe` Just [ 'A' .. 'Z' ]
        prop "fails for numbers outside [0..25]" $ \x -> (x < 0 || x > 25) ==>
            toChar x `shouldBe` Nothing
