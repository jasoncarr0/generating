{-# LANGUAGE NoImplicitPrelude #-}

module Math.SparseSeries 
(

) where
import NumericPrelude

data Indexing i => SpS i a = SpS (i -> a)
