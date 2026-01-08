module Exercises1
  ( nAnd1, nAnd2, nAnd3, nAnd4
  , nDigits
  , nRoots
  , smallerRoot, largerRoot
  , smallerRootCorrect, largerRootCorrect
  , power2
  , mult
  , prod
  , factorial
  ) where

-- Exercise 1  nAnd1 False True

-- Version 1 
nAnd1 :: Bool -> Bool -> Bool
nAnd1 a b = not (a && b)

-- Version 2
nAnd2 :: Bool -> Bool -> Bool
nAnd2 True  True  = False
nAnd2 _     _     = True

-- Version 3
nAnd3 :: Bool -> Bool -> Bool
nAnd3 True  True  = False
nAnd3 True  False = True
nAnd3 False True  = True
nAnd3 False False = True

nAnd4 :: Bool -> Bool -> Bool
nAnd4 a True = not a
nAnd4 a False = True 

-- Exercise 2 nDigits (-98765)
nDigits :: Integer -> Int
nDigits n
  | n < 0     = length (show (abs n))
  | otherwise = length (show n)

-- Exercise 3 nRoots 1 0 (-1) 
nRoots :: Float -> Float -> Float -> Int
nRoots a b c
  | a == 0.0  = error "the first argument should be non-zero!"
  | d  > 0.0  = 2 --du sakniai
  | d == 0.0  = 1 --vienas saknis
  | d  < 0.0  = 0
  where
    d = b*b - 4.0*a*c

-- Exercise 4 smallerRoot 1 (-3) 2 = 1.0 / largerRoot  1 (-3) 2 = 2 / smallerRoot 1 0 1
smallerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c
  | a == 0.0     = error "the first argument should be non-zero!"
  | d <  0.0     = error "no real roots"
  | otherwise    = min r1 r2
  where
    d  = b*b - 4.0*a*c
    sd = sqrt d
    r1 = (-b - sd) / (2.0*a)
    r2 = (-b + sd) / (2.0*a)

largerRoot :: Float -> Float -> Float -> Float
largerRoot a b c
  | a == 0.0     = error "the first argument should be non-zero!"
  | d <  0.0     = error "no real roots"
  | otherwise    = max r1 r2
  where
    d  = b*b - 4.0*a*c
    sd = sqrt d
    r1 = (-b - sd) / (2.0*a)
    r2 = (-b + sd) / (2.0*a)

-- Exercise 4 with nRoots (corrected)
smallerRootCorrect :: Float -> Float -> Float -> Float
smallerRootCorrect a b c
  | n == 0    = error "no real roots"
  | n == 1    = (-b) / (2.0 * a)
  | otherwise = min r1 r2
  where
    n  = nRoots a b c
    d  = b*b - 4.0*a*c
    sd = sqrt d
    r1 = (-b - sd) / (2.0*a)
    r2 = (-b + sd) / (2.0*a)

largerRootCorrect :: Float -> Float -> Float -> Float
largerRootCorrect a b c
  | n == 0    = error "no real roots"
  | n == 1    = (-b) / (2.0 * a)
  | otherwise = max r1 r2
  where
    n  = nRoots a b c
    d  = b*b - 4.0*a*c
    sd = sqrt d
    r1 = (-b - sd) / (2.0*a)
    r2 = (-b + sd) / (2.0*a)

-- Exercise 5 power2 (-3)
power2 :: Integer -> Integer
power2 n
  | n < 0     = 0
  | n == 0    = 1 --rekursijos baze
  | otherwise = 2 * power2 (n - 1) 

-- Exercise 6 mult 7 (-3) 
mult :: Integer -> Integer -> Integer
mult _ 0 = 0 --rekursijos baze
mult m n
  | n  > 0    = m + mult m (n - 1) -- mult 3 4. 3 + mult 3 3 -> 3 + (3 + mult (3 2)) -> 3 + (3+ (3 + mult (3 1))) -> 3 + (3+(3+(3+ mult (3 0)))) -> 3 + 3 + 3 + 3 + 0 = 12
  | n  < 0    = negate (mult m (-n)) -- mult 3 -(4) ->  negate (mult 3 4) -> ... -> negate (-12)

-- Exercise 7 prod 3 6
prod :: Integer -> Integer -> Integer
prod m n
  | m > n     = error "invalid range: m must be <= n"
  | m == n    = m --is vieno skaiciaus m
  | otherwise = prod m (n - 1) * n  -- prod 3 6 ->  prod 3 5 * 6 -> prod 3 4 * 5 -> prod 3 3 * 4 -> prod 3 3 = 3 -> 3 * 4 * 5 * 6

-- factorial factorial 5 
factorial :: Integer -> Integer
factorial 0 = 1
factorial k
  | k < 0     = error "factorial is undefined for negative integers"
 -- | k == 0    = 1 --baze, 0! = 1
  | otherwise = prod 1 k
