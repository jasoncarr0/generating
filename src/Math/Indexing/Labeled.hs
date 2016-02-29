{-# LANGUAGE NoImplicitPrelude #-}

module Math.Indexing.Labeled
(
) where

import NumericPrelude
import qualified Algebra.Additive as Add
import qualified Algebra.ZeroTestable as ZT
import qualified Data.Map as Map
import Math.Indexing


newtype T l a = T (Map.Map l a)

instance (Ord l, ZT.C a, Add.C a) => Add.C (T l a) where
    (T m1) + (T m2) = T $ Map.unionWith (+) m1 m2
    zero = T Map.empty
    negate (T m1) = T (fmap negate m1)
instance (Show l, Show a) => Show (T l a) where
    show (T m1) = drop 1 $ Map.foldrWithKey showTerm "" m1 where
        showTerm l a str = ' ' : ((show l) ++ "^" ++ (show a) ++ str)