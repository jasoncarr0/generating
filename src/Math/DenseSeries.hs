{-# LANGUAGE NoImplicitPrelude #-}

module Math.DenseSeries 
(

) where
import NumericPrelude


data (Indexing i) => DS i a = DS [[a]]