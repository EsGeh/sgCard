{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, DeriveDataTypeable, Rank2Types, ScopedTypeVariables, FlexibleContexts, UndecidableInstances #-}
--{-# OPTIONS_GHC -fglasgow-exts #-}
module SGCard.Unary where
import SGCard.Container
--import SGCard.Card(Card)
--import Util


import Data.Generics

data Zero = Zero deriving(Typeable,Data)
data Succ n = Succ n deriving(Typeable,Data)

class Inc a b | a -> b
--instance Inc Zero (Succ Zero)
--instance Inc (Succ a) (Succ (Succ a))
instance (Container Int n) => Inc n (Succ n)

class (Container Int a, Container Int b) => Add a b c | a b -> c --, a c -> b
instance (Container Int b) => Add Zero b b
instance (Add a b c) => Add (Succ a) b (Succ c)

class (Container Int a, Container Int b, Container Int c) => Mul a b c | a b -> c 
instance (Container Int b) => Mul Zero b Zero
--instance Mul N1 b N1
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

{-
instance Show Zero where
	show n = show $ Card.toInt n
instance (Card n ) => Show (Succ n) where
	show (Succ n) = show $ Card.toInt (Succ n)
-}
instance Container Int Zero where
	fromContainer _ = 0
instance (Container Int n) => Container Int (Succ n) where
	fromContainer _ = succ $ fromContainer (undefined :: n)
