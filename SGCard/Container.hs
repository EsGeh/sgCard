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

instance (Container t l, Container t r) => Container (t,t) (l,r) where
	fromContainer tuple = (fromContainer $ fst tuple, fromContainer $ snd tuple)


{- |this enables compatibility with the "reflection" library.
still experimental...
-}
instance (Reifies config Int) => Container Int (Proxy config) where
	fromContainer x = reflect x

{- examples:
example = reify 10 f
	where
		f :: Reifies config Int => Proxy config -> String
		f n = show $ fromContainer n
example2 :: String
example2 = reify (10,20) f
	where
		f n = show $ fromContainer n

exampleString :: String
exampleString = reify "Hello World!" f
	where
		f n = fromContainer n
-}

{-
newtype StringWrapper = S { fromWrapper :: String }
example2 = reify (S "Hello World!") f
	where
		f n = fromWrapper $ fromContainer n
-}
