{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, DeriveDataTypeable, Rank2Types, ScopedTypeVariables, FlexibleContexts, UndecidableInstances #-}
module SGCard.Unary where

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

withCard :: Int -> (forall n . Container Int n => n -> w) -> w
withCard 0 f = f (undefined :: Zero)
withCard n f = withCard (n-1) (\(_ :: n) -> f (undefined :: Succ n))

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

