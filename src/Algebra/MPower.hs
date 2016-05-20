{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module Algebra.MPower
( C
, mpower
) where

import NumericPrelude
import qualified Algebra.Monoid as Mon

-- | MPower.C defines a Monoid with a more efficient method for powers
class Mon.C a => C a where
    mpower :: a -> Integer -> a

-- | Ambiguous most general instance so we can use the method for any monoid
instance Mon.C a => C a where
    mpower a n = Mon.cumulate $ take (fromInteger n) $ repeat a

