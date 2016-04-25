{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, MultiParamTypeClasses #-}

module Math.SeriesIndex
( C
, eval
, pureI
) where

import NumericPrelude
import qualified Algebra.Additive as Add
import qualified Algebra.Monoid as Mon
import qualified Algebra.Ring as Ring
import qualified Data.Map.Strict as Map

-- | An Indexing i over a label set l defines a method for evaluation
-- that guarantees certain order constraints
class (Ord i, Mon.C i, Ord l) => C i l where
-- | evaluate the element of a label given an embedding into any ring
-- precondition: for an embedding f and a label l, eval l f >= f l
-- furthermore, l1 < l2 -> eval f l1 < eval f l2
-- whenever f preserves order
    eval :: (Ring.C r) => i -> (l -> r) -> r
    pureI :: l -> i

