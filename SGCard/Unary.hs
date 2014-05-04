{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, DeriveDataTypeable, Rank2Types, ScopedTypeVariables, FlexibleContexts, UndecidableInstances #-}
{- |This module provides a way to represent Int values at type level-}
module SGCard.Unary(
	-- *basic types 
	Zero, Succ,
	-- * reflect integers to types (runtime)
	withCard,
	withCard1, withCard2, withCard3, withCard4,
	-- * type arithmetic
	LessThan, Equal, LessOrEqual,
	Inc, Add, Mul, inc, add, mul,
	-- * shortcuts
	n0, n1, n2, n3, n4, n5, n6, n8, n9, 
	N0, N1, N2, N3, N4, N5, N6, N8, N9, 
	
) where

import SGCard.Container
import Data.Generics


data Zero = Zero deriving(Typeable,Data)
data Succ n = Succ n deriving(Typeable,Data)

-- show instances
instance Show Zero where
	show n = show $ fromContainer n
instance ( Container Int n ) => Show (Succ n) where
	show n = show $ fromContainer n

-- container for Integers:
instance Container Int Zero where
	fromContainer _ = 0
instance (Container Int n) => Container Int (Succ n) where
	fromContainer _ = succ $ fromContainer (undefined :: n)

-- compare cardinal numbers: 
class (Container Int n, Container Int m) => LessThan n m 
instance (Container Int n) => LessThan Zero (Succ n)
instance (LessThan n m) => LessThan (Succ n) (Succ m)

class (Container Int n, Container Int m) => Equal n m 
instance Equal Zero Zero
instance (Equal n m) => Equal (Succ n) (Succ m)

class (Container Int n, Container Int m) => LessOrEqual n m 
instance LessOrEqual Zero Zero
instance (Container Int n) => LessOrEqual Zero (Succ n)
instance (LessOrEqual n m) => LessOrEqual (Succ n) (Succ m)

class Inc a b | a -> b
instance (Container Int n) => Inc n (Succ n)

-- simple arithmetic
class (Container Int a, Container Int b) => Add a b c | a b -> c
instance (Container Int b) => Add Zero b b
instance (Add a b c) => Add (Succ a) b (Succ c)

class (Container Int a, Container Int b, Container Int c) => Mul a b c | a b -> c 
instance (Container Int b) => Mul Zero b Zero
instance (Container Int c,Container Int c', Mul a b c, Add c b c') => Mul (Succ a) b c'

inc :: (Inc a b) => a -> b
inc = undefined

add :: (Add a b c) => a -> b -> c
add = undefined

mul :: (Mul a b c) => a -> b -> c
mul = undefined
--}

succ' :: (Container Int n)=>  n -> Succ n
succ' = Succ

{- |\'withCard int f\' executes a function with a \"reified\" Int. This allows for a type to reflect a value fixed at runtime.

So the main purpose of this method is to provide a context, in which a type is defined that reflects an integer.
Notice, that this type can never leave the context of the function f. 
-}
withCard :: Int -> (forall n . Container Int n => n -> w) -> w
withCard 0 f = f (undefined :: Zero)
withCard n f = withCard (n-1) (\(_ :: n) -> f (undefined :: Succ n))


withCard1 :: Int -> (forall t . (Container Int t) => t -> res) -> res
withCard1 x f = withCard x f

withCard2 :: Int -> Int -> (forall l r . (Container Int l, Container Int r) => l -> r -> res) -> res
withCard2 a b f = withCard a (\n1 -> withCard b (f n1))

withCard3 :: Int -> Int -> Int -> (forall t1 t2 t3. (Container Int t1, Container Int t2, Container Int t3) => t1 -> t2 -> t3 -> res) -> res
withCard3 a b c f = withCard a (\n1 -> withCard b (\n2 -> withCard c (f n1 n2)))

withCard4 :: Int -> Int -> Int -> Int -> (forall t1 t2 t3 t4 . (Container Int t1, Container Int t2, Container Int t3, Container Int t4) => t1 -> t2 -> t3 -> t4 -> res) -> res
withCard4 a b c d f = withCard a (\n1 -> withCard b (\n2 -> withCard c (\n3 -> withCard d (f n1 n2 n3))))

type N0 = Zero
type N1 = Succ N0
type N2 = Succ N1
type N3 = Succ N2
type N4 = Succ N3
type N5 = Succ N4
type N6 = Succ N5
type N7 = Succ N6
type N8 = Succ N7
type N9 = Succ N8
n0 :: N0
n1 :: N1
n2 :: N2
n3 :: N3
n4 :: N4
n5 :: N5
n6 :: N6
n7 :: N7
n8 :: N8
n9 :: N9
n0 = Zero
n1 = Succ n0
n2 = Succ n1
n3 = Succ n2
n4 = Succ n3
n5 = Succ n4
n6 = Succ n5
n7 = Succ n6
n8 = Succ n7
n9 = Succ n8
