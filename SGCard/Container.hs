{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module SGCard.Container where

import Data.Reflection
import Data.Proxy

class Container t c | c -> t where
	fromContainer :: c -> t

fromCard :: (Container Int c) => c -> Int
fromCard = fromContainer

instance (Container t l, Container t r) => Container (t,t) (l,r) where
	fromContainer tuple = (fromContainer $ fst tuple, fromContainer $ snd tuple)

-- this enables compatibility with the "reflection" library.
-- still experimental...
instance (Reifies config t) => Container t (Proxy config) where
	fromContainer x = reflect x
{-
instance (Reifies config Int) => Container Int (Proxy config) where
	fromContainer x = reflect x
-}

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
