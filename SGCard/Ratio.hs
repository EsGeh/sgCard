{-# LANGUAGE Rank2Types, ScopedTypeVariables, FlexibleContexts #-}
module SGCard.Ratio (
	FractionClass(..),
	Fraction(),
	frac, withFrac,
) where

import SGCard.Container
import SGCard.Unary
import Data.Ratio

data Fraction num den = CTFrac num den

class (Show f) => FractionClass f where
	toFrac :: f -> Ratio Int
instance (Container Int num, Container Int den) => FractionClass (Fraction num den) where
	toFrac _ = num % den
		where
			num = fromContainer (undefined :: num)
			den = fromContainer (undefined :: den)
instance (Container Int num, Container Int den) => Show (Fraction num den) where
	show f = case denominator (toFrac f) of
		1 -> show $ numerator $ toFrac f
		_ -> show $ toFrac f

frac :: (Container Int num, Container Int den) => (num,den) -> Fraction num den
frac (num,den) = CTFrac num den

withFrac :: Ratio Int -> (forall ctRatio. (FractionClass ctRatio) => ctRatio-> res) -> res
withFrac frac f = withFrac' (numerator frac, denominator frac) f

withFrac' :: (Int,Int) -> (forall num den . (Container Int num, Container Int den) => Fraction num den -> res) -> res
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
