{-# LANGUAGE Rank2Types, ScopedTypeVariables, FlexibleContexts #-}
{- |This module provides a way to represent Rational values at type level-}
module SGCard.Ratio (
	-- * basic types/type classes
	FractionClass(..),
	Fraction(),
	-- * construct compile time fixed Ratios
	frac,
	-- * reflect ratios to types (compile time)
	withFrac,
) where

import SGCard.Container
import SGCard.Unary
import Data.Ratio

-- |represents a fraction of two Int
data Fraction num den = CTFrac num den

{- |The type

@
FractionClass f => f
@

means, that f is a type that represents a value of type \'Ratio Int\'
in other words: f is a "reflection" of a value of type \'Ratio Int\'
-}
class (Show f) => FractionClass f where
	-- |convert back to type level
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

-- |construct a fraction fixed at compile time
frac :: (Container Int num, Container Int den) => (num,den) -> Fraction num den
frac (num,den) = CTFrac num den

{- |\'withCard int f\' executes a function with a \"reified\" \'Ration Int\'. This allows for a type to reflect a value fixed at runtime.

So the main purpose of this method is to provide a context, in which a type is defined that reflects a \'Ratio Int\'.
Notice, that this type can never leave the context of the function f. 
-}
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
