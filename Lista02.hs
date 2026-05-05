-- I
resultado :: Integer
resultado = sum [x^2 | x <- [1..100]]

-- II
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

-- III
quadrado :: Int -> [(Int, Int)]
quadrado n = [(x, y) | (x, y) <- grid n n, x /= y]

-- IV
replicate1 :: Int -> a -> [a]
replicate1 n valor = [valor | _ <- [1..n]]

-- V
pitag :: Int -> [(Int, Int, Int)]
pitag limite = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- VI
fatores :: Int -> [Int]
fatores n = [x | x <- [1..n], n `mod` x == 0]

perfeitos :: Int -> [Int]
perfeitos n = [x | x <- [1..n], sum [y | y <- fatores x, y /= x] == x]

-- VII
pares :: [(Int, Int)]
pares = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]

-- VIII
buscar :: Eq a => a -> [(a, Int)] -> [Int]
buscar x ps = [i | (y,i) <- ps, x == y]

posicoes :: Eq a => a -> [a] -> [Int]
posicoes x xs = buscar x (zip xs [0..])

-- IX
prodEscalar :: [Int] -> [Int] -> Int
prodEscalar xs ys = sum [x * y | (x,y) <- zip xs ys]
