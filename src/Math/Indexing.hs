{-# LANGUAGE NoImplicitPrelude #-}

module Math.Indexing
(
) where

import NumericPrelude
import qualified Algebra.Additive as Add

class Add.C i => C i where
    norm :: i -> Int
    atNorm :: Int -> [i]