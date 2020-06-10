module ListSeq where

import Seq
import Par

emptyL      :: [a]
emptyL = []

singletonL  :: a -> [a]
singletonL x    = [x]

lengthL     :: [a] -> Int
lengthL []      = 0
lengthL (x:xs)  = (lengthL xs) + 1

nthL        :: [a] -> Int -> a
nthL [] _       = error "Invalid index."
nthL (x:xs) 0   = x
nthL (x:xs) n   = nthL xs (n - 1)

tabulateL   :: (Int -> a) -> Int -> [a]
tabulateL f n   | n <= 0    = error "Invalid index."
                | otherwise = tabulateL' f 0 (n - 1)
                    where tabulateL' f a b  | a == b    = [(f a)]
                                            | otherwise = let (x, xs) = f a ||| tabulateL' f (a + 1) b in x : xs

mapL        :: (a -> b) -> [a] -> [b]
mapL _ []       = []
mapL f (x:xs)   = let (x', xs') = f x ||| mapL f xs
                    in x': xs'

filterL     :: (a -> Bool) -> [a] -> [a]
filterL _ []        = []
filterL f (x:xs)    = let (p, xs') = f x ||| filterL f xs
                        in if p then x : xs' else xs'

appendL     :: [a] -> [a] -> [a]
appendL [] ys       = ys
appendL (x:xs) ys   = x : appendL xs ys

takeL       :: [a] -> Int -> [a]
takeL xs 0      = []
takeL [] n      = error "Invalid index."
takeL (x:xs) n  = x:(takeL xs (n - 1))

dropL       :: [a] -> Int -> [a]
dropL xs 0      = xs
dropL [] n      = error "Invalid index."
dropL (x:xs) n  = dropL xs (n - 1)

showtL      :: [a] -> TreeView a [a]
showtL []   = EMPTY
showtL [x]  = ELT x
showtL xs   = let (l', r') = takeL xs (quot l 2) ||| dropL xs (quot l 2) in NODE l' r'
                where l = lengthL xs

showlL      :: [a] -> ListView a [a]
showlL []       = NIL
showlL (x:xs)   = CONS x xs

joinL       :: [[a]] -> [a]
joinL []        = []
joinL (x:xs)    = appendL x (joinL xs)

combineL    :: (a -> a -> a) -> [a] -> [a]
combineL _ []       = []
combineL _ [x]      = [x]
combineL f (x:y:ys) = let (x', xs') = f x y ||| combineL f ys in x':xs'

reduceL :: (a -> a -> a) -> a -> [a] -> a
reduceL f b []      = b
reduceL f b [x]     = f b x
reduceL f b (x:xs)  = reduceL f b (combineL f xs)

contractL   :: (a -> a -> a) -> [a] -> [a]
contractL _ []          = []
contractL _ [x]         = [x]
contractL f (x:y:ys)    = let (y', ys') = f x y ||| contractL f ys in y':ys'

expandL     :: (a -> a -> a) -> [a] -> [a] -> [a]
expandL _ [] _              = []
expandL f [x] [_]           = [x]
expandL f (x:xs) (y:_:ys)   = let (y', ys') = f x y ||| expandL f xs ys in x:y':ys' 

scanL       :: (a -> a -> a) -> a -> [a] -> ([a], a)
scanL f b []    = ([], b)
scanL f b [x]   = ([b], f b x)
scanL f b xs    = let (xs', b') = scanL f b (contractL f xs) in (expandL f xs' xs, b')

fromListL    :: [a] -> [a]
fromListL xs    = xs

instance Seq [] where
    emptyS      = emptyL
    singletonS  = singletonL
    lengthS     = lengthL
    nthS        = nthL
    tabulateS   = tabulateL
    mapS        = mapL
    filterS     = filterL
    appendS     = appendL
    takeS       = takeL
    dropS       = dropL
    showtS      = showtL
    showlS      = showlL
    joinS       = joinL
    reduceS     = reduceL
    scanS       = scanL
    fromList    = fromListL
