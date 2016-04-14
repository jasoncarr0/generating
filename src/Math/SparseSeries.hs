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
    t1 + t2 = mergeWith (+) t1 t2
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

-- | singleton returns the series of a single value at a specific index
singleton :: Ring.C a => i -> T i a
singleton i = T [(i, one)]

-- | merge two sparse series together, combining repeated values with a function
mergeWith :: Ord i => (a -> a -> a) -> T i a -> T i a -> T i a
mergeWith f t1 t2 = mergeWithIndex (\_ x y -> f x y) t1 t2

-- | merge two sparse series together, combining repeated values
-- with a function that also takes as input the index
mergeWithIndex :: Ord i => (i -> a -> a -> a) -> T i a -> T i a -> T i a
mergeWithIndex f (T xs1) (T xs2) = T (mergeWith' f xs1 xs2) where
    mergeWith' f xs1@((i1,a1):xs1') xs2@((i2,a2):xs2')
        | i1 == i2 = (i1, f i1 a1 a2):(mergeWith' f xs1' xs2')
        | i1 > i2 = (i2, a2):(mergeWith' f xs1 xs2')
        | i1 < i2 = (i1, a1):(mergeWith' f xs1' xs2)



excludeZeroes :: ZT.C a => [(i, a)] -> [(i, a)]
excludeZeroes = filter (\(_, x) -> not (isZero x))
