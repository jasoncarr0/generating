{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, TypeOperators, TypeFamilies #-}

module Math.Indexing.Labeled
( T (T)
, enumLabelsBy
) where

import NumericPrelude
import qualified Algebra.Additive as Add
import qualified Algebra.Ring as Ring
import qualified Algebra.ZeroTestable as ZT
import qualified Data.Map.Strict as Map
import GHC.Generics
import Data.MemoTrie (HasTrie, (:->:), trieGeneric, untrieGeneric, enumerateGeneric, Reg, trie, untrie, enumerate)
--import Data.MemoTrie (:->:)

newtype T l a = T (Map.Map l a) deriving (Eq, Generic)
--Map output should be guaranteed to not have 0 in it
instance (Ord l, ZT.C a, Add.C a) => Add.C (T l a) where
    (T m1) + (T m2) = T $ excludeZero $ Map.unionWith (+) m1 m2
    zero = T Map.empty
    negate (T m1) = T (fmap negate m1)
instance (Show l, Show a) => Show (T l a) where
    show (T m1) = drop 1 $ Map.foldrWithKey showTerm "" m1 where
        showTerm l a str = ' ' : ((show l) ++ "^" ++ (show a) ++ str)
instance (ZT.C a) => ZT.C (T l a) where
    isZero (T m) = null $ excludeZero m
--instance (Eq l, Eq a) => Eq (T l a) where
--    (T m1) == (T m2) = m1 == m2
instance (Ord l, Add.C a, Ord a) => Ord (T l a) where
    compare t1@(T m1) t2@(T m2) = let normCompare = compare (norm t1) (norm t2) in
        if normCompare /= EQ then normCompare else
        compare (Map.toAscList m1) (Map.toAscList m2)


-- | enumLabelsBy takes a list of finite lists of labels and produces a list
-- of labeled indices in these variables
-- Each group is treated equally, and each successive group appears later than previous groups
-- The outermost list may be infinite, but each of the groups must be finite
enumLabelsBy :: (Ord x, ZT.C a, Ring.C a) => [[x]] -> [T x a]
enumLabelsBy xs = [1..] >>= (enum' $ zip xs [1..])
    where enum' :: (Ord x, ZT.C a, Ring.C a) => [([x], Int)] -> Int -> [T x a]
          enum' xs 0 = [T (Map.fromList [])]
          enum' xs 1 = map (\i -> T $ Map.singleton i $ fromInteger 1) (fst $ head xs)
          enum' xs i = do (x, j) <- take i xs
                          out <- enum' (take j xs) (i - j)
                          x' <- x
                          return $ out + T (Map.singleton x' $ fromInteger 1)

-- | enumLabelsWeighted takes a list of elements and produces a list of
-- of labeled indices in these variables consistent with a weighting
-- that gives the nth variable weight n
enumLabelsWeighted :: (Ord x, ZT.C a, Ring.C a) => [x] -> [T x a]
enumLabelsWeighted = enumLabelsBy . (map (:[]))



instance (Ord l, HasTrie l, HasTrie a) => HasTrie (T l a) where
    newtype ((T l a) :->: b) = LabelTrie {unLabelTrie :: [(l, a)] :->: b}
    --newtype ((T l a) :->: b) = LabelTrie {unLabelTrie :: ((Map.Map l a), Int :->: b)

    trie f = LabelTrie $ trie (f . T . Map.fromList)
      where unT (T map) = map
    untrie t = (. (Map.assocs . unT)) (untrie $ unLabelTrie t)
      where unT (T map) = map
    enumerate = map (mapFst (T . Map.fromList)) . enumerate . unLabelTrie 
      where mapFst f (a, b) = (f a, b)


        
norm :: Add.C a => T l a -> a
norm (T m) = Map.foldr (+) zero m

excludeZero :: ZT.C a => Map.Map l a -> Map.Map l a
excludeZero = Map.filter (not . isZero)
