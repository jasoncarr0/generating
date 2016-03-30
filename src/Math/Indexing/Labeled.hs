{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, TypeOperators, TypeFamilies #-}

module Math.Indexing.Labeled
( T (T)
) where

import NumericPrelude
import qualified Algebra.Additive as Add
import qualified Algebra.ZeroTestable as ZT
import qualified Data.Map as Map
import GHC.Generics
import Data.MemoTrie (HasTrie, (:->:), trieGeneric, untrieGeneric, enumerateGeneric, Reg, trie, untrie, enumerate)
--import Data.MemoTrie (:->:)

newtype T l a = T (Map.Map l a) deriving Generic
--Anything which outputs a map MUST run excludeZero
instance (Ord l, ZT.C a, Add.C a) => Add.C (T l a) where
    (T m1) + (T m2) = T $ excludeZero $ Map.unionWith (+) m1 m2
    zero = T Map.empty
    negate (T m1) = T (fmap negate m1)
instance (Show l, Show a) => Show (T l a) where
    show (T m1) = drop 1 $ Map.foldrWithKey showTerm "" m1 where
        showTerm l a str = ' ' : ((show l) ++ "^" ++ (show a) ++ str)
instance (ZT.C a) => ZT.C (T l a) where
    isZero (T m) = null $ excludeZero m
instance (Eq l, Eq a) => Eq (T l a) where
    (T m1) == (T m2) = m1 == m2
instance (Ord l, Add.C a, Ord a) => Ord (T l a) where
    compare t1@(T m1) t2@(T m2) = let normCompare = compare (norm t1) (norm t2) in
        if normCompare /= EQ then normCompare else
        compare (Map.toAscList m1) (Map.toAscList m2)

instance (Ord l, HasTrie l, HasTrie a) => HasTrie (T l a) where
    newtype ((T l a) :->: b) = LabelTrie {unLabelTrie :: [(l, a)] :->: b}
    --newtype ((T l a) :->: b) = LabelTrie {unLabelTrie :: ((Map.Map l a), Int :->: b)

    trie f = LabelTrie $ trie (f . T . Map.fromList)
      where unT (T map) = map
-- f :: T l a -> b
-- trie :: (T l a -> b) -> LabelTrie (== [(l, a)] :->: b)
-- trie :: ([(l, a)] -> b) -> [(l, a)] :->: b
-- fromList . f ::
    untrie t = (. (Map.assocs . unT)) (untrie $ unLabelTrie t)
      where unT (T map) = map
-- untrie :: LabelTrie -> (T l a -> b)
-- untrie . unLabelTrie :: LabelTrie -> [(l, a)] -> b
    enumerate = map (mapFst (T . Map.fromList)) . enumerate . unLabelTrie 
      where mapFst f (a, b) = (f a, b)




        
norm :: Add.C a => T l a -> a
norm (T m) = Map.foldr (+) zero m

excludeZero :: ZT.C a => Map.Map l a -> Map.Map l a
excludeZero = Map.filter (not . isZero)
