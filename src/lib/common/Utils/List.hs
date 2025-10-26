{-# LANGUAGE FlexibleInstances #-}

module Utils.List (
    (!?),
    mapInit
) where

import qualified Safe (atMay)

class IndexLike indexLike where
    infixl 9 !?
    (!?) :: [a] -> indexLike -> Maybe a

instance IndexLike Int where
    (!?) = Safe.atMay

instance IndexLike (Maybe Int) where
    list !? maybeIndex = maybeIndex >>= (list !?)

mapInit :: (a -> a) -> [a] -> [a]
mapInit _ [] = []
mapInit _ (x:[]) = [x]
mapInit f xs = map f (init xs) ++ [last xs]