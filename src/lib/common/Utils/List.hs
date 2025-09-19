{-# LANGUAGE FlexibleInstances #-}

module Utils.List (
    (!?)
) where

import qualified Safe (atMay)

class IndexLike indexLike where
    infixl 9 !?
    (!?) :: [a] -> indexLike -> Maybe a

instance IndexLike Int where
    (!?) = Safe.atMay

instance IndexLike (Maybe Int) where
    list !? maybeIndex = maybeIndex >>= (list !?)
