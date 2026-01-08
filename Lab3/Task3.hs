-- Exercise 1
type Position = (Float, Float)
xPos :: Position -> Float
xPos (x, _) = x

yPos :: Position -> Float
yPos (_, y) = y

data Shape = Circle Float Position | Rectangle Float Float Position
    deriving (Show, Ord, Eq)

overlaps :: Shape -> Shape -> Bool
overlaps (Rectangle w1 h1 pos1) (Rectangle w2 h2 pos2) =
    let x1 = xPos pos1
        y1 = yPos pos1
        x2 = xPos pos2
        y2 = yPos pos2
        x1Left = x1 - w1 / 2
        x1Right = x1 + w1 / 2
        y1Top = y1 + h1 / 2
        y1Bottom = y1 - h1 / 2
        x2Left = x2 - w2 / 2
        x2Right = x2 + w2 / 2
        y2Top = y2 + h2 / 2
        y2Bottom = y2 - h2 / 2
    in
        (x1Left < x2Right) && (x1Right > x2Left) && (y1Top > y2Bottom) && (y1Bottom < y2Top)

overlaps (Circle r1 pos1) (Circle r2 pos2) =
    let dx = xPos pos1 - xPos pos2
        dy = yPos pos1 - yPos pos2
    in  dx * dx + dy * dy <= (r1 + r2) * (r1 + r2)

overlaps (Rectangle w h pos1) (Circle r pos2) =
    let
        xRect = xPos pos1
        yRect = yPos pos1

        xLeft = xRect - w / 2
        xRight = xRect + w / 2
        yTop = yRect + h / 2
        yBottom = yRect - h / 2

        xCircle = xPos pos2
        yCircle = yPos pos2

        xNearest = max xLeft (min xCircle xRight)
        yNearest = max yBottom (min yCircle yTop)

        distanceX = xNearest - xCircle
        distanceY = yNearest - yCircle
    in
        (distanceX**2 + distanceY**2) <= r**2

overlaps (Circle r pos1) (Rectangle w h pos2) = overlaps (Rectangle w h pos2) (Circle r pos1)


-- Exercise 2
any1 :: (a -> Bool) -> [a] -> Bool
any1 cond arr = not (null (filter cond arr))

any2 :: (a -> Bool) -> [a] -> Bool
any2 cond arr = foldr (||) False (map cond arr)

all1 :: (a -> Bool) -> [a] -> Bool
all1 cond arr = null (filter (not . cond) arr) --jeigu saras tuscias - True (tai tikrina null)

all2 :: (a -> Bool) -> [a] -> Bool
all2 cond arr = foldr (&&) True (map cond arr)

all3 :: (a -> Bool) -> [a] -> Bool
all3 cond arr = foldr (\x y -> cond x && y) True arr


-- Exercise 3
unzipping :: (a, b) -> ([a], [b]) -> ([a], [b])
unzipping (a, b) (as, bs) = (a : as, b : bs) -- a:as - prideta i sarasa as elementa a i pradzia

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip arr = foldr unzipping ([], []) arr --galima sakytit kad ([], []) tai startas, unzipping tai funkcija kuri priskiriama kiekvienam elementui, arr - sarasas
--foldr eina nuo paskutinio elemento sarase i iki pirmo (iki starto)


-- Exercise 4
lengthMap :: [a] -> Int
lengthMap = sum . map (\_ -> 1) -- \_ -> 1 - daro is kiekvieno elemento 1, map -> kikvienam elementui priskiria funkcija (...), sum . map (\_ -> 1) == lengthMap xs = sum (map (\_ -> 1) xs)

lengthFold :: [a] -> Int
lengthFold = foldr (\_ n -> n + 1) 0 -- \_ n -> n + 1, cia _ - tai  saraso elementas, nereikalingas. Pvz: [5, 8, 10] 1)(\_ n -> n + 1) 10 0 = 1  2)(\_ n -> n + 1) 8 1 = 2...


-- Exercise 5
ff :: Integer -> [Integer] -> Integer
ff maxNum = last . takeWhile (<= maxNum) . scanl (+) 0 . map (*10) . filter (>= 0) --eina is desines i kaire, 


-- Exercise 6
total :: (Integer -> Integer) -> Integer -> Integer
total f n = (sum . map f) [0..n]


-- Exercise 7
iter1 :: Integer -> (a -> a) -> (a -> a)
iter1 n f
    | n <= 0 = id --id tai funkcija kuri ka gavo ta ir grazino, tai baze
    | otherwise = f . iter1 (n - 1) f

iter2 :: Integer -> (a -> a) -> (a -> a)
iter2 n f
    | n <= 0 = id
    | otherwise = foldr (.) id (replicate (fromIntegral n) f) --replicate 2 (+1) == [(+1), (+1)], daro sarasa. replicate (fromIntegral n) f -> replicate 3 f == [f, f, f]


-- Exercise 8
splits :: [a] -> [([a], [a])]
splits xs = [ (take n xs, drop n xs) | n <- [0 .. length xs] ]

-- Testavimas:
-- overlaps (Circle 10 (0,0)) (Circle 1 (0,0))          == True
-- overlaps (Circle 1 (0,0)) (Circle 1 (3,0))           == False
-- any1 even [1,3,5]                                    == False
-- all2 odd [1,3,5]                                     == True
-- myUnzip [(1,'a'),(2,'b')]                            == ([1,2],"ab")
-- myUnzip ([] :: [(Int,Char)])                         == ([],[])
-- lengthMap "hello"                                    == 5
-- lengthFold [1,2,3,4]                                 == 4
-- ff 15  [1,2,3]                                       == 10
-- total (\x -> x) 3                                    == 6
-- (iter1 0 (+1)) 5                                     == 5
-- splits "Spy"