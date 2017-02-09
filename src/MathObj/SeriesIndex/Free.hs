{-# LANGUAGE NoImplicitPrelude, TypeFamilies #-}

module MathObj.SeriesIndex.Free
( T 
, fromList
, freeLabel
, label
) where

import NumericPrelude
import qualified Algebra.Additive as Add
import qualified Algebra.Monoid as Mon
import qualified Algebra.Ring as Ring
import qualified Algebra.ToInteger as ToInt
import qualified Algebra.ZeroTestable as ZT
import qualified Data.Map.Strict as Map
import qualified MathObj.Series as Ser
import qualified MathObj.SeriesIndex as SI

newtype T l = T [l] deriving (Eq, Ord)
instance Mon.C (T l) where
    (T l1) <*> (T l2) = T $ l1 ++ l2
    idt = T []
instance (Show l) => Show (T l) where
    show (T l) = show l
instance (Ord l) => SI.C (T l) where
    type Label (T l) = l
    eval (T ls) f = product $ map (flip f id) ls
    fromLblPow l i = T $ take (fromInteger i) $ repeat l
    getLblPow l (T ls) = sum $ map (\l' -> if (l == l') then 1 else 0) ls

fromList :: [l] -> T l
fromList = T

-- | Construct a freeLabel from the label
freeLabel :: l -> T l 
freeLabel = T . return

-- | Construct a Free.T from the base label 
-- label is exported so the name can be used if only one label module is loaded
label :: l -> T l
label = freeLabel
