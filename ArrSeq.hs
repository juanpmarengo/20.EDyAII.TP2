module ArrSeq where

import Seq
import qualified Arr as A
import Arr ((!))
import Par

emptyA	:: A.Arr a
emptyA  = A.fromList []

singletonA	:: a -> A.Arr a
singletonA x = A.fromList [x]

mapA		:: (a -> b) -> A.Arr a -> A.Arr b
mapA f xs = A.tabulate (\i -> f (xs!i)) (A.length xs)

filterA		:: (a -> Bool) -> A.Arr a -> A.Arr a
filterA f xs = A.flatten (mapA (\x -> if f x then singletonA x else emptyA) xs)

appendA		:: A.Arr a -> A.Arr a -> A.Arr a
appendA xs ys = A.flatten (A.fromList [xs, ys])

takeA		:: A.Arr a -> Int -> A.Arr a
takeA xs n = A.subArray 0 n xs

dropA		:: A.Arr a -> Int -> A.Arr a
dropA xs n = A.subArray n ((A.length xs) - n) xs

showtA		:: A.Arr a -> TreeView a (A.Arr a)
showtA xs	| l == 0	= EMPTY
			| l == 1	= ELT (xs ! 0)
			| otherwise = NODE (takeA xs (quot l 2)) (dropA xs (quot l 2))
				where l = A.length xs

showlA     	:: A.Arr a -> ListView a (A.Arr a)
showlA xs	| A.length xs == 0	= NIL
		 	| otherwise			= CONS (xs ! 0) (dropA xs 1)

combineA		:: (a -> a -> a) -> A.Arr a -> A.Arr a
combineA f xs	| l == 0		= emptyA
				| mod l 2 == 0	= A.tabulate (\i -> f (xs ! (2 * i)) (xs ! ((2 * i) + 1))) (quot l 2)
				| otherwise		= A.tabulate (\i -> if i == quot l 2 then xs ! (2 * i) else f (xs ! (2 * i)) (xs ! ((2 * i) + 1))) ((quot l 2) + 1)
					where l = A.length xs

reduceA		:: (a -> a -> a) -> a -> A.Arr a -> a
reduceA f b xs	| l == 0	= b
				| l == 1	= f b (xs ! 0)
				| otherwise = reduceA f b (combineA f xs)
					where l = A.length xs

scanA		:: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a, a)
scanA f b xs 	| A.length xs == 0	= (emptyS, b)
				| A.length xs == 1	= (singletonS b, f b (xs ! 0))
				| otherwise			= (A.tabulate (\i -> if mod i 2 == 0 then xs' ! (quot i 2) else f (xs' ! (quot i 2)) (xs ! (i - 1))) (A.length xs), b')
					where 	(xs', b') = scanA f b (combineA f xs)

xs = A.fromList [1]

instance Seq A.Arr where
	emptyS		= emptyA
	singletonS	= singletonA
	lengthS		= A.length
	nthS		= (!)
	tabulateS	= A.tabulate
	mapS		= mapA
	filterS		= filterA
	appendS		= appendA
	takeS		= takeA
	dropS		= dropA
	showtS		= showtA
	showlS		= showlA
	joinS		= A.flatten
	reduceS		= reduceA
	scanS		= scanA
	fromList	= A.fromList
