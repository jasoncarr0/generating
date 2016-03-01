{-# LANGUAGE NoImplicitPrelude #-}

module Math.SparseSeries 
( T (T)

) where
import NumericPrelude
import qualified Algebra.Additive as Add
import qualified Algebra.Ring as Ring
import qualified Algebra.ZeroTestable as ZT
import qualified Data.Map as Map

newtype T i a = T (Map.Map i a)

instance Functor (T i) where
    fmap f (T g) = T (fmap f g)
instance (Ord i, ZT.C a, Add.C i, Add.C a) => Add.C (T i a) where
    (T m1) + (T m2) = T $ excludeZero $ Map.unionWith (+) m1 m2
    zero = T Map.empty
    negate (T m1) = T (fmap negate m1)
instance (Show i, Show a) => Show (T i a) where
    show (T m1) = drop 3 $ Map.foldrWithKey showTerm "" m1 where
        showTerm i a str = " + " ++ (show a) ++ (show i) ++ str
instance (Eq i, Eq a) => Eq (T i a) where
    (T m1) == (T m2) = m1 == m2
    
-- TODO: Deal with equal bottoms, best is probably just to cut off
instance (Ord i, Add.C a, Ord a) => Ord (T i a) where
    compare (T m1) (T m2) = compare (Map.toAscList m1) (Map.toAscList m2)
        
        
excludeZero :: ZT.C a => Map.Map l a -> Map.Map l a
excludeZero = Map.filter (not . isZero)