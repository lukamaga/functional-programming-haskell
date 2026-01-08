module Exercises2Solutions where

import Data.Char (isAlpha, toUpper)

--Exercise 1 
average :: [Float] -> Float
average [] = error "average: empty list"
average xs = sum xs / fromIntegral (length xs)

-- Exercise 2
dividesRec :: Integer -> [Integer]
dividesRec 0 = [] -- be galo daug
dividesRec n = dividesFrom 1
  where
    m = abs n
    dividesFrom d --d - dabartinis kandidatas
      | d > m          = [] --rekursijos baze, nera kandidatu
      | m `mod` d == 0 = d : dividesFrom (d + 1) --1 : [2,3,4] -> [1,2,3,4]
      | otherwise      = dividesFrom (d + 1) --kitas kandidatas

dividesLC :: Integer -> [Integer]
dividesLC 0 = [] --be galo daug
dividesLC n = [ d | d <- [1 .. m], m `mod` d == 0 ] --d <- [1 .. m] -> d = 1, d = 2... | 
  where
    m = abs n

isPrime :: Integer -> Bool
isPrime n = n >= 2 && length (dividesLC n) == 2

-- Exercise 3
prefix :: String -> String -> Bool
prefix [] _          = True
prefix _  []         = False
prefix (x:xs) (y:ys) = x == y && prefix xs ys --prefix "abc" "abcdef" tikrinam kol ne bus ('c' == 'c') && prefix "" "def" o tai prefix [] _  = True, po pirmo bus "bc" "bcdef"

substring :: String -> String -> Bool
substring [] _  = True
substring _  [] = False
substring pat txt =
  prefix pat txt || substring pat (tail txt) --tail "hello" = "ello"


-- Exercise 4
permut :: [Integer] -> [Integer] -> Bool
permut xs ys =
  length xs == length ys && allTrue [ count a xs == count a ys | a <- xs ]
  where
    count a zs = length [ z | z <- zs, z == a ] --count 2 [1,2,2,3] = 2, z==a, z bus sarase tik jeigu z==a, kiek elementu a yra saraze zs

    allTrue []     = True
    allTrue (b:bs) = b && allTrue bs

-- Exercise 5
capitalise :: String -> String
capitalise s = [ toUpper c | c <- s, isAlpha c ] --isAlpha - tikrina ar raide, c <- s tikrinam visa eilute s, isAlpha c - filtras

-- Exercise 6
type Basket = [(String, Float)]

itemTotal :: Basket -> Basket
itemTotal [] = []
itemTotal ((k,v):xs) = --k - pavadinimas, v - kaina, xs - kitos prekes
  let total = v + sum [p | (k',p) <- xs, k' == k] --p - visos kainos, sum[0.5] = 0.5, iskom poras su tokia pat preke 
      rest  = [ (k',p) | (k',p) <- xs, k' /= k ] -- visos prekes isskirus prekes su pavadinimu k
  in (k,total) : itemTotal rest 

itemDiscount :: String -> Integer -> Basket -> Basket
itemDiscount target pct basket = --target -prekes pavadinimas kur padaryti nuolaida, pct - nuolaida, 
  let p = max 0 (min 100 pct) --jeigu <0 -> daro 0, jeigu >100 - daro 100, visada nuo 0 iki 100
      factor = 1.0 - (fromIntegral p / 100.0)
  in [ if name == target then (name, price * factor)  else (name, price) | (name, price) <- basket ]


-- average [1,2,3,4]                    == 2.5
-- dividesRec 12                        == [1,2,3,4,6,12]
-- dividesLC 12                         == [1,2,3,4,6,12]
-- isPrime 2                            == True
-- isPrime 15                           == False
-- prefix "abc" "abcdef"                == True
-- substring "cde" "abcdef"             == True
-- substring "" "anything"              == True
-- permut [1,2,2,3] [2,3,2,1]           == True
-- capitalise "He110, Wo_rld!"          == "HEWORLD"
-- itemTotal [("apple",1.0),("banana",2.0),("apple",0.5)]
--   == [("apple",1.5),("banana",2.0)]
-- itemDiscount "apple" 20 [("apple",1.0),("banana",2.0),("apple",3.0)]
--   == [("apple",0.8),("banana",2.0),("apple",2.4)]

--be go, be map, rekursijos ir comp...

