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
instance (Ord i, ZT.C i, Add.C i, Add.C a) => Add.C (T i a) where
    (T m1) + (T m2) = T $ Map.unionWith (+) m1 m2
    zero = T Map.empty
    negate (T m1) = T (fmap negate m1)
instance (Show l, Show a) => Show (T l a) where
    show (T m1) = drop 3 $ Map.foldrWithKey showTerm "" m1 where
        showTerm i a str = " + " ++ (show a) ++ (show i) ++ str