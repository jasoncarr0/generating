{-# LANGUAGE NoImplicitPrelude #-}

module Indexing
(
) where

import NumericPrelude
import Algebra.Additive

class Algebra.Additive.C i => C i where
    norm :: i -> Int
    all :: Int -> [i]
