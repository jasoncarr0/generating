{-# LANGUAGE NoImplicitPrelude #-}

module Math.SparseSeries 
( T (T)

) where
import NumericPrelude
import qualified Algebra.Additive as Add
import qualified Algebra.Ring as Ring
import qualified Algebra.ZeroTestable as ZT
import qualified Data.Map as Map

newtype T i a = T [(i, a)]

instance Functor (T i) where
    fmap f (T g) = T (fmap (fmap f) g)
instance (Ord i, ZT.C a, Add.C i, Add.C a) => Add.C (T i a) where
    (T m1) + (T m2) = T $ undefined
    zero = T []
    negate (T m1) = T (fmap (fmap negate) m1)
instance (Show i, Show a) => Show (T i a) where
    show (T m1) = drop 3 $ foldr showTerm "" m1 where
        showTerm (i, a) str = " + " ++ (show a) ++ (show i) ++ str
-- | Currently comparison and equality of two equal and infinite series will
-- result in bottom
instance (Eq i, Eq a) => Eq (T i a) where
    (T m1) == (T m2) = m1 == m2
    
-- | Currently comparison and equality of two equal and infinite series will
-- result in bottom
instance (Ord i, Add.C a, Ord a) => Ord (T i a) where
    compare (T m1) (T m2) = compare m1 m2
       
