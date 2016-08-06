{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module MathObj.SparseSeries 
( T (T)
, compose'
, mergeWith
, mergeWithIndex
, multWith
, star
, expS
) where
import NumericPrelude
import qualified Algebra.Additive as Add
import qualified Algebra.Field as Field
import Algebra.Monoid ((<*>))
import qualified Algebra.Monoid as Mon
import qualified Algebra.MPower as MPow
import qualified Algebra.Ring as Ring
import qualified Algebra.ZeroTestable as ZT
import qualified Data.Map as Map
import qualified MathObj.SeriesIndex as SI
import qualified MathObj.SeriesIndex.Labeled as SIL
import qualified MathObj.SeriesIndex.Subscripted as SIS
import qualified MathObj.Series as Srs


-- | A value of type T contains and list of index value pairs
-- sorted to be nondecreasing as the list is traversed, that is
-- the lowest values of the index are always near the front
newtype T i a = T [(i, a)]

instance Functor (T i) where
    fmap f (T g) = T (fmap (fmap f) g)
instance (Ord i, Mon.C i, ZT.C a, Add.C a) => Add.C (T i a) where
    t1 + t2 = mergeWith (+) t1 t2
    zero = T []
    negate (T m1) = T (fmap (fmap negate) m1)
instance (Ord i, ZT.C a) => ZT.C (T i a) where
    isZero (T l) = null l -- zero terms should be removed by other operations
instance (Ord i, Mon.C i, ZT.C a, Ring.C a) => Ring.C (T i a) where
    one = singleton Mon.idt
    fromInteger n = T [(Mon.idt, fromInteger n)]
    t1 * t2 = multWith (\(_, x1) (_, x2) -> x1 * x2) t1 t2
instance (Ord i, Mon.C i, ZT.C a, Ring.C a) => Srs.C (T i a) where
    type Index (T i a) = i
    type Coeff (T i a) = a
    mapIndex f (T ps) = T $ joinRepeats $ map (\(i,a) -> (f i, a)) ps
    compose (T ts) t1 l =  T $ joinRepeats $ compose' func ts where
      func l' f
        | l == l' = (Srs.mapIndex f) t1
        | otherwise = T [(SI.fromLbl l', one)]
      compose' func [] = []
      compose' func ((i, x):ts) = (i2, x*x2) : 
        (map (fmap (x*)) $ mergeLWith (+) ts2 $ compose' func ts) where
          (T ((i2, x2):ts2)) = SI.eval i func
    fromIndexPower i p = T [(i, p)]
instance (Show i, Show a) => Show (T i a) where
    show (T m1) = drop 3 $ foldr showTerm "" m1 where
        showTerm (i, a) str = " + " ++ (show a) ++ (show i) ++ str
-- | Currently comparison and equality of two equal and infinite series will
-- result in bottom
instance (Eq i, Eq a) => Eq (T i a) where
    (T m1) == (T m2) = m1 == m2
-- | Currently comparison and equality of two equal and infinite series will
-- result in bottom
instance (Ord i, Add.C a, Ord a) => Ord (T i a) where
    compare (T m1) (T m2) = compare m1 m2


-- | Compose two series where the label of one type is unit without forcing
-- a label to be specified
compose' :: (SI.C i, ZT.C a, Ring.C a, SI.Label i ~ ()) => T i a -> T i a -> T i a
compose' t1 t2 = Srs.compose t1 t2 ()


-- | Utility function to join repeated indices
joinRepeats :: (Eq i, Ring.C a) => [(i, a)] -> [(i, a)]
joinRepeats [] = []
joinRepeats [x] = [x]
joinRepeats ((i1,x1):(i2,x2):xs)
    | i1 == i2 = joinRepeats $ (i1, x1 + x2):xs
    | otherwise = (i1, x1) : (joinRepeats $ (i2,x2):xs)



-- | merge two sparse series together, combining repeated values with a function
mergeWith :: (ZT.C a, Ord i) => (a -> a -> a) -> T i a -> T i a -> T i a
mergeWith f = mergeWithIndex $ const f

-- | merge two sparse series together, combining repeated values
-- with a function that also takes as input the index
mergeWithIndex :: (ZT.C a, Ord i) => 
    (i -> a -> a -> a) -> T i a -> T i a -> T i a
mergeWithIndex f (T xs1) (T xs2) = T (excludeZeroes $ mergeLWithIndex f xs1 xs2)

mergeLWith :: (Ord i) => (a -> a -> a) -> [(i, a)] -> [(i, a)] -> [(i, a)]
mergeLWith f = mergeLWithIndex $ const f

-- | merge the internal list representation of the series
mergeLWithIndex :: (Ord i) => (i -> a -> a -> a) -> [(i, a)] -> [(i, a)] ->[(i, a)]
mergeLWithIndex f [] xs2 = xs2
mergeLWithIndex f xs1 [] = xs1
mergeLWithIndex f xs1@((i1,a1):xs1') xs2@((i2,a2):xs2')
        | i1 < i2 = (i1, a1):(mergeLWithIndex f xs1' xs2)
        | i1 > i2 = (i2, a2):(mergeLWithIndex f xs1 xs2')
        | otherwise = (i1, f i1 a1 a2):(mergeLWithIndex f xs1' xs2')

-- | merges two series by taking pairwise each possible set of indices
-- that sum to the correct value, applying a function to each pair
multWith :: (Mon.C i, Ord i, ZT.C a, Add.C a) =>
    ((i, a) -> (i, a) -> a) -> T i a -> T i a -> T i a
multWith f (T ts1) (T ts2) = T $ excludeZeroes $ multWith' f ts1 ts2 where
    multWith' f [] _ = []
    multWith' f _ [] = []
    multWith' f ((i1, x1):ts1) ((i2, x2):ts2) = ((i1 <*> i2), f (i1, x1) (i2, x2)) : mergeLWithIndex (\_ a b -> a + b) (map (appF f (i1, x1)) ts2) (multWith' f ts1 ((i2, x2):ts2))
    appF f (i1, x1) (i2, x2) = (i1 <*> i2, f (i1, x1) (i2, x2))

-- | remove all zeroes from the internal representation of the series
-- used only internally
excludeZeroes :: ZT.C a => [(i, a)] -> [(i, a)]
excludeZeroes = filter (\(_, x) -> not (isZero x))

-- | get the value at a given index
(!!) :: Eq i => T i a -> i -> a
(T ts) !! i = snd $ head $ dropWhile (\(i', _) -> i /= i') ts



-- | Generates the series 1 + x + x^2 + x^3 + ...  for a given label
star :: (Ring.C a, MPow.C i) => i -> T i a
star i = T $ map (\n -> (i `MPow.mpower` n, one)) [0..]
    

-- | Generates the series 1 + x + x^2/2 + x^3/6 + x^4/24 + ...
-- for a given label and field coefficients
expS :: (Field.C a, MPow.C i) => i -> T i a 
expS i = T $ zipWith (\n f -> (i `MPow.mpower` (fromInteger n), one / (fromInteger f))) 
    [0..] fact
    where fact = one:zipWith (*) fact [one..]

