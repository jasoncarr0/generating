{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, MultiParamTypeClasses #-}

module Math.Indexing.Labeled
( T (T)
, enumLabelsBy
, enumLabelsWeighted
, fromList
) where

import NumericPrelude
import qualified Algebra.Additive as Add
import qualified Algebra.Ring as Ring
import qualified Algebra.Module as Module
import qualified Algebra.ZeroTestable as ZT
import qualified Data.Map.Strict as Map

newtype T l a = T (Map.Map l a) deriving (Eq)
instance (Ord l, ZT.C a, Add.C a) => Add.C (T l a) where
    (T m1) + (T m2) = T $ excludeZero $ Map.unionWith (+) m1 m2
    zero = T Map.empty
    negate (T m1) = T (fmap negate m1)

instance (Show l, Show a) => Show (T l a) where
    show (T m1) = drop 1 $ Map.foldrWithKey showTerm "" m1 where showTerm l a str = ' ' : ((show l) ++ "^" ++ (show a) ++ str)
instance (ZT.C a) => ZT.C (T l a) where
    isZero (T m) = null $ excludeZero m
--instance (Eq l, Eq a) => Eq (T l a) where
--    (T m1) == (T m2) = m1 == m2
instance (Ord l, Add.C a, Ord a) => Ord (T l a) where
    compare t1@(T m1) t2@(T m2) = if normCompare /= EQ then normCompare else
        compare (Map.toAscList m1) (Map.toAscList m2) where 
            normCompare = compare (norm (flip const) t1) (norm (flip const) t2)




-- | enumLabelsBy takes a list of finite lists of labels and produces a list
-- of labeled indices in these variables
-- Each group is treated equally, and each successive group appears later than previous groups
-- The outermost list may be infinite, but each of the groups must be finite
enumLabelsBy :: (Ord l, ZT.C a, Ring.C a) => [[l]] -> [T l a]
enumLabelsBy xs = [1..] >>= (enum' $ zip xs [1..])
    where enum' :: (Ord l, ZT.C a, Ring.C a) => [([l], Int)] -> Int -> [T l a]
          enum' xs 0 = [T (Map.fromList [])]
          enum' xs 1 = map (\i -> T $ Map.singleton i $ fromInteger 1) (fst $ head xs)
          enum' xs i = do (x, j) <- take i xs
                          out <- enum' (take j xs) (i - j)
                          x' <- x
                          return $ out + T (Map.singleton x' $ fromInteger 1)

-- | enumLabelsWeighted takes a list of elements and produces a list of
-- of labeled indices in these variables consistent with a weighting
-- that gives the nth variable weight n
enumLabelsWeighted :: (Ord l, ZT.C a, Ring.C a) => [l] -> [T l a]
enumLabelsWeighted = enumLabelsBy . (map (:[]))

-- | fromList creates a labeled index from a list of pairs
fromList :: Ord l => [(l, a)] -> T l a
fromList = T . Map.fromList

norm :: Add.C a => (l -> a -> a) -> T l a -> a
norm f (T m) = Map.foldrWithKey (\k x y -> (f k x) + y) zero m

excludeZero :: ZT.C a => Map.Map l a -> Map.Map l a
excludeZero = Map.filter (not . isZero)
