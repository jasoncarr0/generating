{-# LANGUAGE NoImplicitPrelude #-}

module Math.SparseSeries 
(

) where
import NumericPrelude
import qualified Algebra.Ring as Ring
import qualified Data.Map as Map

newtype SpS i a = SpS (Map.Map i a)

instance Functor (SpS i) where
	fmap f (SpS g) = SpS (fmap f g)
