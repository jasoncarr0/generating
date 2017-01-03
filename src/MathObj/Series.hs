{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module MathObj.Series
( C
, Index
, Coeff
, mapIndex
, fromIndexPower
, compose
, term
) where

import NumericPrelude
import qualified Algebra.Ring as Ring
import qualified Algebra.Field as Field
import qualified MathObj.SeriesIndex as SI

class (Ring.C s, SI.C (Index s)) => C s where
    type Index s :: *
    type Coeff s :: *
    -- | maps a nondecreasing function of the indices through a series
    mapIndex :: (Index s -> Index s) -> s -> s
    -- | makes a series of a single term with the given index and coefficient
    fromIndexPower :: Index s -> Coeff s -> s
    -- | compose two series, replacing each instance of a given index by
    -- the second series whenever it appears
    compose :: s -> s -> (SI.Label (Index s)) -> s

term :: (C s, Ring.C (Coeff s), SI.C (Index s)) => SI.Label (Index s) -> s
term l = fromIndexPower (SI.fromLbl l) one

x :: (C s, Ring.C (Coeff s), SI.C (Index s), SI.Label (Index s) ~ Char) => s
x = term 'x'


