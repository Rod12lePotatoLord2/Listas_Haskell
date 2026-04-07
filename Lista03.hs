{-# ANN module "HLint: ignore Use foldr" #-}
-- I
fatorial :: Int -> Int
fatorial n 
    | n == 0    = 1
    | n > 0     = n * fatorial (n - 1)
    | otherwise = error "Número negativo não permitido"

-- II
somar :: Int -> Int
somar 0 = 0
somar n = n + somar (n - 1)

-- III
potencia :: Int -> Int -> Int
potencia m 0 = 1
potencia m n = m * potencia m (n - 1)

-- IV
euclides :: Int -> Int -> Int
euclides m n
    | m == n    = m
    | m > n     = euclides (m - n) n
    | otherwise = euclides m (n - m)

-- V: Letra a
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

-- Letra b
concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

-- Letra c
replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x : replicate' (n - 1) x

-- Letra d
at :: [a] -> Int -> a
(x:_)  `at` 0 = x
(_:xs) `at` n = xs `at` (n - 1)

-- Letra e
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs)
    | y == x    = True
    | otherwise = elem' y xs

-- VI
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- VII
metades :: [a] -> ([a], [a])
metades xs = splitAt (length xs `div` 2) xs

mergesort :: Ord a => [a] -> [a]
mergesort []  = []
mergesort [x] = [x]
mergesort xs  = merge (mergesort esq) (mergesort dir)
  where
    (esq, dir) = metades xs

-- VIII: Letra a
soma :: [Int] -> Int
soma []     = 0
soma (x:xs) = x + soma xs

-- Letra b
comprimento :: [a] -> Int
comprimento []     = 0
comprimento (_:xs) = 1 + comprimento xs

-- Letra c
ultimo :: [a] -> a
ultimo [x]    = x
ultimo (_:xs) = ultimo xs
ultimo []     = error "Lista vazia não tem último elemento"
