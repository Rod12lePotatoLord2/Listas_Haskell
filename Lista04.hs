-- Lista 04: Funções de Alta Ordem

-- 1. Lista de Inteiros Ímpares crescente
impares :: [Int] -> [Int]
impares xs = ordenar (filter odd xs)

ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar (x:xs) =
    ordenar (filter (< x) xs) ++ [x] ++ ordenar (filter (>=x) xs)

-- 2. Enésima posição em lista
posicao :: Int -> [a] -> a
posicao n lista = head (foldl combinador [] lista)
  where
    combinador acc x
      | length acc <= n = x : acc
      | otherwise       = acc

-- 3. Repetição de valor em formato de Lista (Sem Replicate)
repetir :: Int -> [[Int]]
repetir n = map (\x -> take x (repeat x)) [n, n-1 .. 1]

-- 4. Verificar Palíndromo
verificarPalindromo :: Eq a => [a] -> Bool
verificarPalindromo lista = lista == reverse lista

-- 5. N elementos de Fibonacci
fib :: Int -> [Integer]
fib n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- 6. Definindo Funções
-- a) All
al1 :: (a -> Bool) -> [a] -> Bool
al1 _ []     = True
al1 p (x:xs) = p x && al1 p xs
-- b) Any
any2 :: (a -> Bool) -> [a] -> Bool
any2 _ []     = False
any2 p (x:xs) = p x || any2 p xs
-- c) TakeWhile
takeWh1le :: (a -> Bool) -> [a] -> [a]
takeWh1le _ []     = []
takeWh1le p (x:xs)
  | p x       = x : takeWh1le p xs
  | otherwise = []
-- d) DropWhile
dropWh1le :: (a -> Bool) -> [a] -> [a]
dropWh1le _ []     = []
dropWh1le p (x:xs)
  | p x       = dropWh1le p xs
  | otherwise = x:xs

-- 7. Map e Filter com Foldr
mapa :: (a -> b) -> [a] -> [b]
mapa f = foldr (\x acc -> f x : acc) []

filtro :: (a -> Bool) -> [a] -> [a]
filtro p = foldr (\x acc -> if p x then x : acc else acc) []

-- 8. Foldl para conversão de inteiros
dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> acc * 10 + x) 0
