{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, MultiParamTypeClasses #-}

module Math.SeriesIndex.Free
( T 
) where

import NumericPrelude
import qualified Algebra.Additive as Add
import qualified Algebra.Monoid as Mon
import qualified Algebra.Ring as Ring
import qualified Algebra.ToInteger as ToInt
import qualified Algebra.ZeroTestable as ZT
import qualified Data.Map.Strict as Map
import qualified Math.SeriesIndex as Index

newtype T l = T [l] deriving (Eq, Ord)
instance Mon.C (T l) where
    (T l1) <*> (T l2) = T $ l1 ++ l2
    idt = T []
instance (Show l) => Show (T l) where
    show (T l) = show l
instance (Ord l) => Index.C (T l) l where
    eval (T l) f = product $ map f l
    pureI l = T $ [l]


