{-# LANGUAGE NoImplicitPrelude, TypeFamilies, FlexibleContexts #-}

module MathObj.SeriesIndex
( C
, Label
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
class (Ord i, Mon.C i, Ord (Label i)) => C i where
    type Label i :: *
-- | evaluate the element of a label given an embedding of the label
-- into a ring with a mapping for 
-- condition: eval l f >= f (fromLbLPow l one)
-- furthermore,   l1 < l2   implies   eval l1 f < eval l2 f
-- whenever f preserves order
    eval :: (Ring.C r) => i -> (Label i -> (i -> i) -> r) -> r
-- | Make an index from a label and a power
    fromLblPow :: Label i -> Integer -> i
-- | Find the power of a given label in the index
-- getLblPow l . fromLblPow l = id
    getLblPow :: Label i -> i -> Integer

fromLbl :: C i => Label i -> i
fromLbl l = fromLblPow l one
