{-# LANGUAGE FlexibleContexts #-}
module Main where

import SGCard
import Data.Reflection


main = do
	print $ test2
	print $ testAdd
	print $ testVec
	print $ testWithCard

test2 = show $ (undefined :: N2)
testAdd = show $ (undefined :: Add N2 N3 res => res)

newtype Vec n a = Vec { fromVec :: [a] }
	deriving( Show )

v :: Container Int n => n -> [a] -> Vec n a
v n list =
	if checkLength (fromContainer n) list
	then Vec $ take (fromContainer n) list
	else error "too few values!"

checkLength size list = length (take size list) == size

addV :: (Num a, Container Int n) => Vec n a -> Vec n a -> Vec n a
addV (Vec a) (Vec b) = Vec $ zipWith (+) a b

vL = (v n3 [1,2,3])
vR = (v n3 [4,5,6])

testVec = addV vL vR

-- reflect values to types at compile time:

testWithCard = withCard 10 f

f :: Container Int n => n -> [Int]
f n = fromVec $	-- type n cannot leave this context...
	v n [1..] `addV` v n [1..]
