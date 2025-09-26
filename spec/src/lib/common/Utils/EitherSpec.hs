module Utils.EitherSpec where

import Utils.Either

import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = describe "Spec" $ do
    describe "??" $ do
        prop "keeps `Right`s" $ \(r :: Int) ->
            (Right r ?? 0) == r
        prop "null-coaleses `Left`" $ \(l :: Int) (r :: Int) ->
            (Left l ?? r) == r
