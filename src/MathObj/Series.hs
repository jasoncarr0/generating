{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module MathObj.Series
( C (..)
, star
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
    -- | add an nondecreasing list of series with only finitely many nonzero values
    -- at any given index. This should never diverge for series meeting the precondition
    sumSeq :: [s] -> s

star :: C s => s -> s
star s = sumSeq $ map (s ^) [0..]
