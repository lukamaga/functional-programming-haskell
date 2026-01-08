module Exercise4 where
import Prelude hiding ((<*>))

-- Exercise 1

data GTree a = Leaf a | Gnode [GTree a]
  deriving (Show, Eq)

depth :: GTree a -> Int
depth (Leaf _) = 0 --gylis 0, nes medis tai lapas tai mum nesvarbu kas yra lape
depth (Gnode []) = 0 --jei mazgas Gnode neturi vaiku, mes laikome kad tai 0 lygis + apsauga
depth (Gnode xs) = 1 + maximum (map depth xs) --xs - vaiku sarasas, pvz: xs = [Leaf 1, Gnode [Leaf 2, Leaf 3]], map depth xs = [depth (Leaf 1), depth (Gnode [Leaf 2, Leaf 3])] = [1, 2]

occurs :: Eq a => a -> GTree a -> Bool
occurs x (Leaf y) = x == y --jei medis yra lapas, patikriname ar ieškomas elementas x yra lygus lapo reikšmei y
occurs x (Gnode xs) = any (occurs x) xs --jei medis tai mazgas, tikriname ar bent viename vaiko medyje pasitaiko elementas x

mapGTree :: (a -> b) -> GTree a -> GTree b
mapGTree f (Leaf x) = Leaf (f x) -- Jei medis yra lapas (Leaf), pritaikome funkciją f lapo reikšmeiir grąžiname naują lapą
mapGTree f (Gnode xs) = Gnode (map (mapGTree f) xs) -- Jei medis yra mazgas (Gnode), mes NEKEIČIAME paties mazgo, bet rekursyviai pritaikome mapGTree kiekvienam vaikui

-- Exercise 2 padariau be lookup, nes kazkodel nesuveikia su jo:(

data Expr a = Lit a | EVar Var | Op (Ops a) [Expr a]
type Ops a = [a] -> a
type Var = Char
type Valuation a = [(Var, a)]

findVar :: Eq a => Var -> Valuation a -> a
findVar v [] = error ("Variable " ++ [v] ++ " not found")
findVar v ((v', val):vs)
    | v == v' = val
    | otherwise = findVar v vs

eval :: Eq a => Valuation a -> Expr a -> a
eval _ (Lit x) = x
eval values (EVar v) = findVar v values
eval values (Op f exprs) = f (map (eval values) exprs)

-- Exercise 3

type RegExp = String -> Bool

epsilon :: RegExp
epsilon = (=="")

char :: Char -> RegExp
char ch = (==[ch])

(|||) :: RegExp -> RegExp -> RegExp
e1 ||| e2 = \x -> e1 x || e2 x

(<*>) :: RegExp -> RegExp -> RegExp   
e1 <*> e2 = \x ->
    or [e1 y && e2 z | (y,z) <- splits x]
    where
        splits :: [a] -> [([a],[a])]
        splits lst = [splitAt i lst | i <- [0..length lst]]

star :: RegExp -> RegExp             
star p = epsilon ||| (p <*> star p)

option :: RegExp -> RegExp            
option p = epsilon ||| p

plus :: RegExp -> RegExp             
plus p = p <*> star p

-- Exercise 4

data Result a = OK a | Error String
  deriving (Show, Eq)

composeResult :: (a -> Result b) -> (b -> Result c) -> (a -> Result c)
composeResult f g x = case f x of
                        Error s -> Error s
                        OK y    -> g y

-- Exercise 5

primes :: [Integer]
primes = sieve [2..]

sieve :: Integral a => [a] -> [a]
sieve (x:xs) =
    x : sieve [y | y <- xs, y `mod` x > 0]

goldbach :: Integer -> Bool
goldbach n = and [e `elem` primeSums | e <- evens]
    where
        evens = [4, 6 .. n]
        ps = takeWhile (<= n) primes
        primeSums = [p1 + p2 | p1 <- ps, p2 <- ps, p1 + p2 <= n]

-- Exercise 6

data Stream a = Cons a (Stream a)

streamtoList :: Stream a -> [a]
streamtoList (Cons x xs) = x : streamtoList xs

streamIterate :: (a -> a) -> a -> Stream a
streamIterate f x = Cons x (streamIterate f (f x))

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x xs) ys = Cons x (streamInterleave ys xs)

-- depth (Gnode [Leaf 1, Gnode [Leaf 2, Leaf 3]])                         == 2
-- occurs 2 (Gnode [Leaf 1, Gnode [Leaf 2]])                              == True
-- eval [('x', 10)] (Op sum [EVar 'x', Lit 5])                            == 15
-- (option (char 'a')) "a"                                                == True.
-- composeResult (\x -> OK (x+1)) (\y -> OK (y*2)) 5                      == OK 12
-- goldbach 50                                                            == True
-- take 5 (streamtoList (streamIterate (+1) 0))                           == [0,1,2,3,4]
-- take 6 (streamtoList (streamInterleave (streamIterate id 1) (streamIterate id 2))) == [1,2,1,2,1,2]