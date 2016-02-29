{-# LANGUAGE NoImplicitPrelude #-}

module Math.SparseSeries 
(

) where
import NumericPrelude
import qualified Algebra.Ring as Ring

data SpS i a = SpS (i -> a)

instance Functor (SpS i) where
	fmap f (SpS g) = SpS (fmap f g)
