-- AV I Haskell --

-- 1.
intercalar :: a -> [a] -> [a]
intercalar _ [] = []
intercalar x (y:ys) = y : x : intercalar x ys

-- 2.
frequencia :: Eq a => a -> [a] -> Int
frequencia _ [] = 0
frequencia x (y:ys)
            | x == y = 1 + frequencia x ys
            | otherwise = frequencia x ys

-- 3.
substituir :: Eq a => a -> a -> [a] -> [a]
substituir _ _ [] = []
substituir ant nov (x:xs)
            | x == ant = nov : substituir ant nov xs
            | otherwise = x : substituir ant nov xs

-- 4.
totalEstoque :: [(String, Int, Float)] -> Float
totalEstoque [] = 0.0
totalEstoque ((_, q, v):xs) = fromIntegral q * v + totalEstoque xs

-- 5.
quantidades :: (Num b, Eq a) => [a] -> [(a, b)]
quantidades [] = []
quantidades (x: xs) = (x, 1 + contar x xs) : quantidades (dropSe (==x) xs)

contar :: (Num a, Eq t) => t -> [t] -> a
contar x [] = 0
contar x (y:ys)
             | x == y = 1 + contar x ys
             | otherwise = contar x ys

dropSe :: (a -> Bool) -> [a] -> [a]
dropSe f l = [ x | x <- l, not (f x)]
