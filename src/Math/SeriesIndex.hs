{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, MultiParamTypeClasses #-}

module Math.SeriesIndex
( C
, eval
, fromLblPow
, fromLbl
, getLblPow
) where

import NumericPrelude
import qualified Algebra.Additive as Add
import qualified Algebra.Monoid as Mon
import qualified Algebra.Ring as Ring
import qualified Data.Map.Strict as Map

-- | An Indexing i over a label set l defines a method for evaluation
-- that guarantees certain order constraints
class (Ord i, Mon.C i, Ord l) => C i l where
-- | evaluate the element of a label given an embedding of the label
-- into a ring with a mapping for 
-- condition: eval l f >= f (fromLbLPow l one)
-- furthermore,   l1 < l2   implies   eval l1 f < eval l2 f
-- whenever f preserves order
    eval :: (Ring.C r) => i -> (l -> (i -> i) -> r) -> r
-- | Make an index from a label and a power
    fromLblPow :: l -> Integer -> i
-- | Find the power of a given label in the index
-- getLblPow l . fromLblPow l = id
    getLblPow :: l -> i -> Integer

fromLbl :: C i l => l -> i
fromLbl l = fromLblPow l one
