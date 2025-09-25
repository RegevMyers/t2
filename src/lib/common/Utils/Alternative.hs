module Utils.Alternative where

import Control.Applicative (Alternative(..))

try :: Alternative f => (a -> f a) -> a -> f a
try f a = f a <|> pure a