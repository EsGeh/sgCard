{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
module SGCard.Ratio (
	Fraction(),
	frac, withFrac,
) where

import SGCard.Card
import SGCard.Unary
import Data.Ratio

data Fraction num den = CTFrac num den

class (Show f) => FractionClass f where
	toFrac :: f -> Ratio Int
instance (Card num, Card den) => FractionClass (Fraction num den) where
	toFrac _ = num % den
		where
			num = toInt (undefined :: num)
			den = toInt (undefined :: den)
instance (Card num, Card den) => Show (Fraction num den) where
	show f = case denominator (toFrac f) of
		1 -> show $ numerator $ toFrac f
		_ -> show $ toFrac f

frac :: (Card num, Card den) => (num,den) -> Fraction num den
frac (num,den) = CTFrac num den

withFrac :: Ratio Int -> (forall ctRatio. (FractionClass ctRatio) => ctRatio-> res) -> res
withFrac frac f = withFrac' (numerator frac, denominator frac) f

withFrac' :: (Int,Int) -> (forall num den . (Card num, Card den) => Fraction num den -> res) -> res
withFrac' frac f = case frac of
	(0,0) -> f (CTFrac n0 n0)
	(num,0) -> withFrac' ((num -1) , 0) (\(_ :: Fraction num den) -> f (undefined :: Fraction (Succ num) den))
	(0,1) -> f (CTFrac n0 n1)
	(num,1) -> withFrac' ((num -1) , 1) (\(_ :: Fraction num den) -> f (undefined :: Fraction (Succ num) den))
	(num,den) -> withFrac' (num , (den-1)) (\(_ :: Fraction num den) -> f (undefined :: Fraction num (Succ den)))

{-
ctFracFromFrac :: Ratio Int -> Fraction num den
ctFracFromFrac frac = ctFrac
-}
