
{-# LANGUAGE EmptyDataDecls, TypeFamilies, MultiParamTypeClasses, 
             FlexibleInstances, UndecidableInstances #-}

module Test where

import qualified ABC as A (test, nice, Maybe, maybe, just,(=>>))

-- utility function: returns n's neighbours in ns
neighbours :: (Eq a) => [a] -> a -> [a]
neighbours ns n =
  [ns `i` (idx-1),ns `i` (idx+1)] -- the two neighbours
  where idx       = fromJust $ n `elemIndex` ns -- index of element
        xs `i` n' = xs !! (n' `mod` (length xs)) -- looks up at index (wraps)

foo (x:xs) = undefined
foo (x:y:xs) = undefined
foo ((abc:def):rest) = undefined
abc (x := y) = undefined


-- TODO: asdfkjh

data LNIL
data LCONS a b

data TTRUE
data TFALSE

instance Show LNIL where show = const "lnil"
instance Show (LCONS a b) where show = const "lcons"
instance Show TTRUE where show = const "ttrue"
instance Show TFALSE where show = const "tfalse"

class CMEMBER a b c
instance CMEMBER LNIL b TFALSE
instance CMEMBER (LCONS a b) a TTRUE
instance (CMEMBER b c ret) => CMEMBER (LCONS a b) c ret

-- type family MEMBER a b
-- type instance MEMBER (LNIL) b = TFALSE
-- type instance MEMBER (LCONS a b) c = MEMBER b c

$(mkEditor ''CMEMBER)

top level ''CMEMBER

foo' :: String
foo' (AS asdf) = "test"

data a :+: b = a :+: b

(Hs 2) + (Hs 4) = asdf 
((Hs 2):xs) + (Hs 4) = undefined
(a :+: b) + (asdf) = undefined
(Hs _) => (Hs 4) = undefined
((A a):x) =|= (T x) = undefined
a * b = asdf
a \ b = asdf
a + b = asf
a .-> b = sdf
a .=> b = a + b
a .= b = a + b
a .=> b= a + b
a .=> b =a + b
a .= b= a + b
a .= b=a + b
a .= b =a + b
a => b = asdf

x (a :+: b) = undefined
(a :+: b) `test` (c :+: d) = undefined

a =|= b = undefined
a |== c = undefined
(*) a b = asf
(=>) s b = asfd

a =>> b = a + b
a ==> b = a + b
a >>= b = asdf

a .=> b = (a + b)
a .= b = (a + b)
a .=> b=(a + b)
a .=> b =[a + b]
a .= b= (a + b)
a .= b=(a + b)
a .= b =(a + b)
A .=> B = undefined

(+) :: Int -> Int -> Int
[] + [] = asdf
_  + A = 

fib :: Int -> Int -> Int
fib {} = 
fib [] = asdf
fib () = asdf

foo n@(Bar a) = a


foo = a `bar` b
a `foo` b = c `bar` d
a `test` (Bat a) = a + b

(o `Foo` m) `test` a = o + m + a
a `test` (o `Foo` m) = a + m + o
(o `Foo` m) `test` (g `Bar` as) = a `FooBar` saf
(Foo a b) `test` (Bar k l) = asdf
f@(Foo a b) `test` b@(Bar k l) = asdf

_foo = "saf" 

myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs
 
myLast' = foldr1 (const id)
 
myLast'' = head . reverse

myButLast :: [a] -> a
myButLast = last . init
 
myButLast' x = reverse x !! 1
 
myButLast'' [x,_]  = x
myButLast'' (_:xs) = myButLast xs

(!!)                :: [a] -> Int -> a
(x:_)  !! 0         =  x
(_:xs) !! n         =  xs !! (n-1)

elementAt :: [a] -> Int -> a
elementAt list i = list !! (i-1)

myLength           :: [a] -> Int
myLength []        =  0
myLength (_:xs)    =  1 + myLength xs

myLength :: [a] -> Int
myLength =  foldr (\x n -> n + 1) 0

reverse          :: [a] -> [a]
reverse          =  foldl (flip (:)) []

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

reverse :: [a] -> [a]
reverse list = reverse' list []
  where
    reverse' [] reversed     = reversed
    reverse' (x:xs) reversed = reverse' xs (x:reversed)

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

data NestedList a = Elem a | List [NestedList a]
 
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

compress :: Eq a => [a] -> [a]
compress = map head . group

compress [] = []
compress [a] = [a]
compress (x : y : xs) = (if x == y then [] else [x]) ++ compress (y : xs)

pack [] = []
pack (x:xs) = let (first,rest) = span (==x) xs
               in (x:first) : pack rest

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:first) : pack rest
         where
           getReps [] = ([], [])
           getReps (y:ys)
                   | y == x = let (f,r) = getReps ys in (y:f, r)
                   | otherwise = ([], (y:ys))
           (first,rest) = getReps xs

encode xs = map (\x -> (length x,head x)) (group xs)

encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . group

encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (length &&& head) $ group xs

randStr = "test"

strFn :: [Char] -> [Char]
strFn "" = "test"
strFn "paper" = "touch the skin"
strFn xs = xs

(ad) + (asdf) = asdf
"saf" + "asdf" = asdf asdf
asdf + "asdf" = asdf
"asdf" + asdf = asdf
'a' + 123 = 'a'
"asf" `asdf` asdf = asdf
"asf" `asdf` "asdf" = asdf
asdf `asdf` "asdf" = asdf
'a' `asd` asdf = 'a'

-- see same colour as main
main = undefined

