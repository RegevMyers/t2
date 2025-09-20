{-# LANGUAGE ScopedTypeVariables #-}

module Utils.MaybeSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Utils.Maybe ((??))

spec :: Spec
spec = describe "Spec" $ do
    describe "??" $ do
        prop "keeps `Just`s" $ \(x :: Int) ->
            (Just x ?? 0) == x
        prop "null-coaleses `Nothing`" $ \(x :: Int) ->
            (Nothing ?? x) == x
