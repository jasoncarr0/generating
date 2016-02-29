{-# LANGUAGE NoImplicitPrelude #-}

module Math.Indexing.Labeled
( T (T)
) where

import NumericPrelude
import qualified Algebra.Additive as Add
import qualified Algebra.ZeroTestable as ZT
import qualified Data.Map as Map


newtype T l a = T (Map.Map l a)

instance (Ord l, ZT.C a, Add.C a) => Add.C (T l a) where
    (T m1) + (T m2) = T $ Map.unionWith (+) m1 m2
    zero = T Map.empty
    negate (T m1) = T (fmap negate m1)
instance (Show l, Show a) => Show (T l a) where
    show (T m1) = drop 1 $ Map.foldrWithKey showTerm "" m1 where
        showTerm l a str = ' ' : ((show l) ++ "^" ++ (show a) ++ str)
instance (Eq l, Eq a) => Eq (T l a) where
    (T m1) == (T m2) = m1 == m2
instance (Ord l, Add.C a, Ord a) => Ord (T l a) where
    compare t1@(T m1) t2@(T m2) = let normCompare = compare (norm t1) (norm t2) in
        if normCompare /= EQ then normCompare else
        compare (Map.toAscList m1) (Map.toAscList m2)
        
norm :: Add.C a => T l a -> a
norm (T m) = Map.foldr (+) zero m