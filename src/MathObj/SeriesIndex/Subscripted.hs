{-# LANGUAGE NoImplicitPrelude, TypeFamilies #-}

module MathObj.SeriesIndex.Subscripted
( T 
, fromList
, multSubscript
) where

import NumericPrelude
import qualified Algebra.Additive as Add
import qualified Algebra.Monoid as Mon
import qualified Algebra.Ring as Ring
import qualified Algebra.ToInteger as ToInt
import qualified Algebra.ZeroTestable as ZT
import qualified Data.Foldable as Fold
import qualified Data.Map.Strict as Map
import qualified MathObj.SeriesIndex as Index




-- | An indexing which maps to each pair of label and subscript a power
newtype T l a = T (Map.Map (l, a) a) deriving (Eq)



instance (Ord l, Ord a, ZT.C a, Add.C a) => Mon.C (T l a) where
    (T m1) <*> (T m2) = T $ excludeZero $ Map.unionWith (+) m1 m2
    idt = T Map.empty
instance (Show l, Show a) => Show (T l a) where
    show (T m1) = drop 1 $ Map.foldrWithKey showTerm "" m1 where 
        showTerm (l, s) a str = ' ' : ((show l) ++ (show s) ++ "^" ++ (show a) ++ str)
instance (ZT.C a) => ZT.C (T l a) where
    isZero (T m) = null $ excludeZero m
instance (Ord l, Add.C a, Ord a) => Ord (T l a) where
    compare t1@(T m1) t2@(T m2) = if normCompare /= EQ then normCompare else
        compare (Map.toAscList m1) (Map.toAscList m2) where 
            normCompare = compare (norm (flip const) t1) (norm (flip const) t2)
instance Fold.Foldable (T l) where
    foldr f x (T m) = foldr f x m

instance (Ord l, ZT.C a, ToInt.C a, Ring.C a) => Index.C (T l a) where
    type Label (T l a) = l
    eval (T m) f = Map.foldrWithKey doPower one m where
      doPower (l, s) a1 r = (f l (multSubscript s))^(toInteger a1) * r
    fromLblPow l n = T $ Map.singleton (l, one) (fromInteger n)
    getLblPow l (T m) = Map.foldr (+) 0 $ Map.mapWithKey 
        (\(l', s) n -> if l' == l then toInteger n else 0) m


-- | Multiplies the subscript of a label by a factor
multSubscript :: Ring.C a => a -> T l a -> T l a
multSubscript s = lift1 $ Map.mapKeysMonotonic (fmap (s*))


-- | Lift from the internal representation to the indexing
lift0 :: Map.Map (l, a) a -> T l a
lift0 = T

-- | Lift a function in one argument over the internal representation 
-- to one over the indexing
lift1 :: (Map.Map (l, a) a -> Map.Map (l, a) a) -> T l a -> T l a
lift1 f (T m) = T (f m)

-- | Lift a function in two arguments over the internal representation 
-- to one over the indexing
lift2 :: (Map.Map (l, a) a -> Map.Map (l, a) a -> Map.Map (l, a) a) -> T l a -> T l a -> T l a
lift2 f (T m1) (T m2) = T (f m1 m2)

-- | fromList creates a subscripted index from a list of pairs
fromList :: (Ord l, Ord a) => [((l, a), a)] -> T l a
fromList = T . Map.fromList

-- | Use a norming function on the label and the two values, then add up each value
norm :: Add.C a => ((l, a) -> a -> a) -> T l a -> a
norm f (T m) = Map.foldrWithKey (\k x y -> (f k x) + y) zero m

-- | Remove all zeroes present in the Map
excludeZero :: ZT.C a => Map.Map (l, a) a -> Map.Map (l, a) a
excludeZero = Map.filter (not . isZero)
