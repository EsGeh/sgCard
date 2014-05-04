{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{- |basic types classes for representing values at type level-}
module SGCard.Container where

import Data.Reflection
import Data.Proxy

{- |The type

@
Container t n => n
@

means, that n is a type that represents a value of type t
in other words: n is a "reflection" of a value of type t
-}
class Container t c | c -> t where
	-- |convert back to type level
	fromContainer :: c -> t 

-- |just an alias for 'fromContainer' for the case in which c reflects an Int
fromCard :: (Container Int c) => c -> Int
fromCard = fromContainer

instance (Container t1 l, Container t2 r) => Container (t1,t2) (l,r) where
	fromContainer tuple = (fromContainer $ fst tuple, fromContainer $ snd tuple)


{- |this enables compatibility with the "reflection" library.
still experimental...
-}
instance (Reifies config Int) => Container Int (Proxy config) where
{-
instance (Reifies config t) => Container t (Proxy config) where
	-- :: proxy config -> t
-}
	fromContainer x = reflect x

reify1 x = reify x

reify2 :: a -> b -> (forall l r . (Container a l, Container b r) => l -> r -> res) -> res
reify2 a b f = reify a (\n1 -> reify b (f n1))

reify3 :: a -> b -> c -> (forall t1 t2 t3. (Container a t1, Container b t2, Container c t3) => t1 -> t2 -> t3 -> res) -> res
reify3 a b c f = reify a (\n1 -> reify b (\n2 -> reify c (f n1 n2)))

reify4 :: a -> b -> c -> d -> (forall t1 t2 t3 t4 . (Container a t1, Container b t2, Container c t3, Container d t4) => t1 -> t2 -> t3 -> t4 -> res) -> res
reify4 a b c d f = reify a (\n1 -> reify b (\n2 -> reify c (\n3 -> reify d (f n1 n2 n3))))
{-
instance (Reifies config Int) => Container Int (Proxy config) where
	fromContainer x = reflect x
-}


{-
newtype StringWrapper = S { fromWrapper :: String }
example2 = reify (S "Hello World!") f
	where
		f n = fromWrapper $ fromContainer n
-}
