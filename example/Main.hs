{-# LANGUAGE FlexibleContexts, Rank2Types, ScopedTypeVariables #-}
module Main where

import SGCard

import Data.Maybe(fromJust)

main = do
	-- create a fixed size list:
	print $ "fromList n3 [1,2,3]:"
	let list1 = fromList n3 [1,2,3]
	print $ list1

	-- only works, if the list has the right length:
	print $ "fromList n3 [1,2,3,4]:"
	print $ fromList n3 [1,2,3,4]

	-- consTS adds one element to the head:
	print $ "consTS 0 $ fromJust (fromList n3 [1,2,3]):"
	print $ consTS 0 $ fromJust list1


-- type safe lists:
{- Explanation:
 - By using a phantom type 'n' we can encode the size of a list into its type.
 - The phantom Type must implement the type class 'sgCard'.
 - This means it represents a natural number.
 -}

data ListTS n a = ListTS { fromListTS :: [a] }
instance (Show a, Card n) => Show (ListTS n a) where
	show listTS =
		"n" ++ (show $ toInt (undefined :: n))
		++ (show $ fromListTS listTS)

fromList :: Card n => n -> [a] -> Maybe (ListTS n a)
fromList n list =
	if toInt n == length list
	then Just $ ListTS list
	else Nothing

consTS :: (Card n) => a -> ListTS n a -> ListTS (Succ n) a
consTS fst = ListTS . (fst:) . fromListTS
