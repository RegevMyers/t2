{-# LANGUAGE ScopedTypeVariables #-}

module Utils.FunctionSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Utils.Function (pipe, (|>>), (<<|))

spec :: Spec
spec = describe "Spec" $ do
    describe "pipe" $ do
        prop "chains " $ \(x :: Int) ->
            pipe [succ, succ] x == (succ . succ) x
    describe "|>>" $ do
        prop "is the infix form of `pipe`" $ \(x :: Int) ->
            (x |>> [succ, succ]) == pipe [succ, succ] x
    describe "<<|" $ do
        prop "is inverse to `|>>`" $ \(x :: Int) ->
            ([succ, succ] <<| x) == (x |>> [succ, succ])
