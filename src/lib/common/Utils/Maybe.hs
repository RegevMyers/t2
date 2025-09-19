module Utils.Maybe (
    (??)
) where

import qualified Data.Maybe as Maybe (fromMaybe)

infixl 1 ??
(??) :: Maybe a -> a -> a
a ?? b = Maybe.fromMaybe b a
